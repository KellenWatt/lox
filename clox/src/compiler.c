#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"
#include "vm.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;


typedef void (*ParseFn)(Parser*, Scanner*, bool);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_METHOD,
    TYPE_INITIALIZER,
    TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

static ParseRule* getRule(TokenType);


static void errorAt(Parser* parser, Token* token, const char* message) {
    if(parser->panicMode) return;
    fprintf(stderr, "[line %d] Error", token->line);
    if(token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // no-op
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser->hadError = true;
}

static void error(Parser* parser, const char* message) {
    errorAt(parser, &parser->previous, message);
}

static void errorAtCurrent(Parser* parser, const char* message) {
    errorAt(parser, &parser->current, message);
}

static void advance(Parser* parser, Scanner* scanner) {
    parser->previous = parser->current;

    for (;;) {
        parser->current = Scanner_scanToken(scanner);
        if(parser->current.type != TOKEN_ERROR) break;
        errorAtCurrent(parser, parser->current.start);
    }
}


static void consume(Parser* parser, Scanner* scanner, TokenType type, const char* message) {
    if(parser->current.type == type) {
        advance(parser, scanner);
        return;
    }

    errorAtCurrent(parser, message);
}

static bool check(Parser* parser, TokenType type) {
    return parser->current.type == type;
}

static bool match(Parser* parser, Scanner* scanner, TokenType type) {
    if(!check(parser, type)) return false;
    advance(parser, scanner);
    return true;
}

Compiler* current = NULL;
ClassCompiler* currentClass = NULL;

static void Compiler_init(struct Compiler* compiler, Parser* parser, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if(type != TYPE_SCRIPT) {
        current->function->name = copyString(parser->previous.start, parser->previous.length);
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if(type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

/* Chunk* compilingChunk; */
static Chunk* currentChunk() {
    return &current->function->chunk;
    /* return compilingChunk; */
}

static uint8_t makeConstant(Parser* parser, Value value) {
    int constant = Chunk_addConstant(currentChunk(), value);
    if(constant > UINT8_MAX)  {
        error(parser, "Too many constants in one chunk");
        return 0;
    }

    return (uint8_t) constant;
}

static void emitByte(Parser* parser, uint8_t byte) {
    Chunk_write(currentChunk(), byte, parser->previous.line);
}

static void emitBytes(Parser* parser, uint8_t byte1, uint8_t byte2) {
    emitByte(parser, byte1);
    emitByte(parser, byte2);
}

static int emitJump(Parser* parser, uint8_t instruction) {
    emitByte(parser, instruction);
    emitByte(parser, 0xff);
    emitByte(parser, 0xff);
    return currentChunk()->count - 2;
}

static void emitLoop(Parser* parser, int loopStart) {
    emitByte(parser, OP_LOOP);
    int offset = currentChunk()->count - loopStart + 2;
    if(offset > UINT16_MAX) error(parser, "Loop body too large");
    emitByte(parser, (offset >> 8) & 0xff);
    emitByte(parser, offset & 0xff);
}

static void patchJump(Parser* parser, int offset) {
    int jump = currentChunk()->count - offset - 2;

    if(jump > UINT16_MAX) {
        error(parser, "Too much code to jump over");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void emitConstant(Parser* parser, Value value) {
    emitBytes(parser, OP_CONSTANT, makeConstant(parser, value));
}

static void emitReturn(Parser* parser) {
    if(current->type == TYPE_INITIALIZER) {
        emitBytes(parser, OP_GET_LOCAL, 0);
    } else {
        emitByte(parser, OP_NIL);
    }
    emitByte(parser, OP_RETURN);
}

static ObjFunction* endCompiler(Parser* parser) {
    emitReturn(parser);
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if(!parser->hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void expression(Parser*, Scanner*);
static void statement(Parser*, Scanner*);
static void declaration(Parser*, Scanner*);

static void parsePrecedence(Parser* parser, Scanner* scanner, Precedence precedence) {
    advance(parser, scanner);
    ParseFn prefixRule = getRule(parser->previous.type)->prefix;
    if(prefixRule == NULL) {
        error(parser, "Expect expression");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(parser, scanner, canAssign);

    while (precedence <= getRule(parser->current.type)->precedence) {
        advance(parser, scanner);
        ParseFn infixRule = getRule(parser->previous.type)->infix;
        infixRule(parser, scanner, canAssign);
    }

    if(canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        error(parser, "Invalid assignment target");
    }
}

static uint8_t identifierConstant(Parser* parser, Token* name) {
    return makeConstant(parser, OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if(a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int addUpvalue(Compiler* compiler, Parser* parser, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for(int i=0; i<upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if(upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error(parser, "Too many closure variables in function");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveLocal(Compiler* compiler, Parser* parser, Token* name) {
    for(int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if(identifiersEqual(name, &local->name)) {
            if(local->depth == -1) {
                error(parser, "Can't read local variable in its own initializer");
            }
            return i;
        }
    }

    return -1;
}

static int resolveUpvalue(Compiler* compiler, Parser* parser, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, parser, name);
    if(local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, parser, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, parser, name);
    if(upvalue != -1) {
        return addUpvalue(compiler, parser, (uint8_t)upvalue, false);
    }

    return -1;
}

static void number(Parser* parser, Scanner* scanner, bool canAssign) {
    double value = strtod(parser->previous.start, NULL);
    emitConstant(parser, NUMBER_VAL(value));
}

static void grouping(Parser* parser, Scanner* scanner, bool canAssign) {
    expression(parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after expression");
}

static void unary(Parser* parser, Scanner* scanner, bool canAssign) {
    TokenType operatorType = parser->previous.type;

    parsePrecedence(parser, scanner, PREC_UNARY);

    switch(operatorType) {
        case TOKEN_BANG: emitByte(parser, OP_NOT); break;
        case TOKEN_MINUS: emitByte(parser, OP_NEGATE); break;
        default: return; // unreachable
    }
}

static void binary(Parser* parser, Scanner* scanner, bool canAssign) {
    TokenType operatorType = parser->previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(parser, scanner, (Precedence)rule->precedence + 1);

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitBytes(parser, OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(parser, OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(parser, OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(parser, OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(parser, OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(parser, OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:    emitByte(parser, OP_ADD); break;
        case TOKEN_MINUS:   emitByte(parser, OP_SUBTRACT); break;
        case TOKEN_STAR:    emitByte(parser, OP_MULTIPLY); break;
        case TOKEN_SLASH:   emitByte(parser, OP_DIVIDE); break;
        default: return; // unreachable
    }
}

static uint8_t argumentList(Parser* parser, Scanner* scanner) {
    uint8_t argCount = 0;
    if(!check(parser, TOKEN_RIGHT_PAREN)) {
        do {
            expression(parser, scanner);
            if(argCount == 255) {
                error(parser, "Can't have more than 255 arguments");
            }
            argCount++;
        } while(match(parser, scanner, TOKEN_COMMA));
    }
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after arguments");
    return argCount;
}

static void call(Parser* parser, Scanner* scanner, bool canAssign) {
    uint8_t argCount = argumentList(parser, scanner);
    emitBytes(parser, OP_CALL, argCount);
}

static void dot(Parser* parser, Scanner* scanner, bool canAssign) {
    consume(parser, scanner, TOKEN_IDENTIFIER, "Expect property name after '.'");
    uint8_t name = identifierConstant(parser, &parser->previous);

    if(canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        expression(parser, scanner);
        emitBytes(parser, OP_SET_PROPERTY, name);
    } else if (match(parser, scanner, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList(parser, scanner);
        emitBytes(parser, OP_INVOKE, name);
        emitByte(parser, argCount);
    } else {
        emitBytes(parser, OP_GET_PROPERTY, name);
    }
}

static void literal(Parser* parser, Scanner* scanner, bool canAssign) {
    switch (parser->previous.type) {
        case TOKEN_FALSE: emitByte(parser, OP_FALSE); break;
        case TOKEN_TRUE: emitByte(parser, OP_TRUE); break;
        case TOKEN_NIL: emitByte(parser, OP_NIL); break;
        default: return; //unreachable
    }
}

static void string(Parser* parser, Scanner* scanner, bool canAssign) {
    emitConstant(parser, OBJ_VAL(copyString(parser->previous.start+1, parser->previous.length - 2)));
}

static void namedVariable(Parser* parser, Scanner* scanner, Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, parser, &name);
    if(arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if((arg = resolveUpvalue(current, parser, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(parser, &name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    /* uint8_t arg = identifierConstant(parser, &name); */
    if(canAssign && match(parser, scanner, TOKEN_EQUAL)) {
        expression(parser, scanner);
        emitBytes(parser, setOp, (uint8_t)arg);
    } else {
        emitBytes(parser, getOp, (uint8_t)arg);
    }
    /* emitBytes(parser, OP_GET_GLOBAL, arg); */
}

static void variable(Parser* parser, Scanner* scanner, bool canAssign) {
    namedVariable(parser, scanner, parser->previous, canAssign);
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void super_(Parser* parser, Scanner* scanner, bool canAssign) {
    if (currentClass == NULL) {
        error(parser, "Can't use 'super' outside of a class");
    } else if (!currentClass->hasSuperclass) {
        error(parser, "Can't use 'super' in a class with no superclass");
    }

    consume(parser, scanner, TOKEN_DOT, "Expect '.' after super");
    consume(parser, scanner, TOKEN_IDENTIFIER, "Expect superclass method name");
    uint8_t name = identifierConstant(parser, &parser->previous);

    namedVariable(parser, scanner, syntheticToken("this"), false);
    if(match(parser, scanner, TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList(parser, scanner);
        namedVariable(parser, scanner, syntheticToken("super"), false);
        emitBytes(parser, OP_SUPER_INVOKE, name);
        emitByte(parser, argCount);
    } else {
        namedVariable(parser, scanner, syntheticToken("super"), false);
        emitBytes(parser, OP_GET_SUPER, name);
    }
}

static void this_(Parser* parser, Scanner* scanner, bool canAssign) {
    if(currentClass == NULL) {
        error(parser, "Can't use 'this' outside of a class");
        return;
    }
    variable(parser, scanner, canAssign);
}

static void expression(Parser* parser, Scanner* scanner) {
    parsePrecedence(parser, scanner, PREC_ASSIGNMENT);
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope(Parser* parser) {
    current->scopeDepth--;

    while(current->localCount> 0 && current->locals[current->localCount-1].depth > current->scopeDepth) {
        if(current->locals[current->localCount - 1].isCaptured) {
            emitByte(parser, OP_CLOSE_UPVALUE);
        } else {
            emitByte(parser, OP_POP);
        }
        /* emitByte(parser, OP_POP); */
        current->localCount--;
    }
}

static void block(Parser* parser, Scanner* scanner) {
    while(!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
        declaration(parser, scanner);
    }

    consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after block");
}


static void synchronize(Parser* parser, Scanner* scanner) {
    parser->panicMode = false;

    while(parser->current.type != TOKEN_EOF) {
        if (parser->previous.type == TOKEN_SEMICOLON) return;
        switch(parser->current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:
                ; //no-op
        }
    }

    advance(parser, scanner);
}

static void addLocal(Parser* parser, Token name) {
    if(current->localCount == UINT8_COUNT) {
        error(parser, "Too many local variables in function");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static void markInitialized() {
    if(current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void declareVariable(Parser* parser) {
    if (current->scopeDepth == 0)  return;

    Token* name = &parser->previous;

    for(int i=current->localCount - 1; i>=0; i--) {
        Local* local = &current->locals[i];
        if(local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if(identifiersEqual(name, &local->name)) {
            error(parser, "Alreday a variable with this name in this scope");
        }
    }

    addLocal(parser, *name);
}

static uint8_t parseVariable(Parser* parser, Scanner* scanner, const char* errorMessage) {
    consume(parser, scanner, TOKEN_IDENTIFIER, errorMessage);
    declareVariable(parser);
    if(current->scopeDepth > 0) return 0;
    return identifierConstant(parser, &parser->previous);
}

static void defineVariable(Parser* parser, uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }
    emitBytes(parser, OP_DEFINE_GLOBAL, global);
}

static void function(Parser* parser, Scanner* scanner, FunctionType type) {
    Compiler compiler;
    Compiler_init(&compiler, parser, type);
    beginScope();

    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after function name");

    if(!check(parser, TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if(current->function->arity > 255) {
                errorAtCurrent(parser, "Can't have more than 255 parameters");
            }
            uint8_t constant = parseVariable(parser, scanner, "Expect parameter name");
            defineVariable(parser, constant);
        } while(match(parser, scanner, TOKEN_COMMA));
    }

    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after parameters");
    consume(parser, scanner, TOKEN_LEFT_BRACE, "Expect '{' before function body");
    block(parser, scanner);

    ObjFunction* function = endCompiler(parser);
    emitBytes(parser, OP_CLOSURE, makeConstant(parser, OBJ_VAL(function)));

    for(int i = 0; i < function->upvalueCount; i++) {
        emitByte(parser, compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(parser, compiler.upvalues[i].index);
    }
    /* emitBytes(parser, OP_CONSTANT, makeConstant(parser, OBJ_VAL(function))); */
}

static void method(Parser* parser, Scanner* scanner) {
    consume(parser, scanner, TOKEN_IDENTIFIER, "Expect method name");
    uint8_t constant = identifierConstant(parser, &parser->previous);

    FunctionType type = TYPE_METHOD;
    if(parser->previous.length == 4 && memcmp(parser->previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(parser, scanner, type);
    emitBytes(parser, OP_METHOD, constant);
}

static void classDeclaration(Parser* parser, Scanner* scanner) {
    consume(parser, scanner, TOKEN_IDENTIFIER, "Expect class name");
    Token className = parser->previous;
    uint8_t nameConstant = identifierConstant(parser, &parser->previous);
    declareVariable(parser);
    emitBytes(parser, OP_CLASS, nameConstant);
    defineVariable(parser, nameConstant);

    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    if(match(parser, scanner, TOKEN_LESS)) {
        consume(parser, scanner, TOKEN_IDENTIFIER, "Expect superclass name");
        variable(parser, scanner, false);

        if(identifiersEqual(&className, &parser->previous)) {
            error(parser, "A class can't inherit from itself");
        }

        beginScope();
        addLocal(parser, syntheticToken("super"));
        defineVariable(parser, 0);

        namedVariable(parser, scanner, className, false);
        emitByte(parser, OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    namedVariable(parser, scanner, className, false);
    consume(parser, scanner, TOKEN_LEFT_BRACE, "Expect '{' before class body.");

    while(!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
        method(parser, scanner);
    }

    consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(parser, OP_POP);

    if(classCompiler.hasSuperclass) {
        endScope(parser);
    }

    currentClass = currentClass->enclosing;
}

static void funDeclaration(Parser* parser, Scanner* scanner) {
    uint8_t global = parseVariable(parser, scanner, "Expect function name");
    markInitialized();
    function(parser, scanner, TYPE_FUNCTION);
    defineVariable(parser, global);
}

static void and_(Parser* parser, Scanner* scanner, bool canAssign) {
    int endJump = emitJump(parser, OP_JUMP_IF_FALSE);
    emitByte(parser, OP_POP);
    parsePrecedence(parser, scanner, PREC_AND);

    patchJump(parser, endJump);
}

static void or_(Parser* parser, Scanner* scanner, bool canAssign) {
    // This could (and maybe should) be its own instruction.
    int elseJump = emitJump(parser, OP_JUMP_IF_FALSE);
    int endJump = emitJump(parser, OP_JUMP);

    patchJump(parser, elseJump);
    emitByte(parser, OP_POP);

    parsePrecedence(parser, scanner, PREC_OR);
    patchJump(parser, endJump);
}

static void varDeclaration(Parser* parser, Scanner* scanner) {
    uint8_t global = parseVariable(parser, scanner, "Expect variable name");

    if(match(parser, scanner, TOKEN_EQUAL)) {
        expression(parser, scanner);
    } else {
        emitByte(parser, OP_NIL);
    }
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after variable declaration");

    defineVariable(parser, global);
}

static void printStatement(Parser* parser, Scanner* scanner) {
    expression(parser, scanner);    
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after value");
    emitByte(parser, OP_PRINT);
}

static void returnStatement(Parser* parser, Scanner* scanner) {
    if(current->type == TYPE_SCRIPT) {
        error(parser, "Can't return from top-level code");
    }
    if(match(parser, scanner, TOKEN_SEMICOLON)) {
        emitReturn(parser);
    } else {
        if(current->type == TYPE_INITIALIZER) {
            error(parser, "Can't return a value from an initializer");
        }
        expression(parser, scanner);
        consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after return value");
        emitByte(parser, OP_RETURN);
    }
}

static void expressionStatement(Parser* parser, Scanner* scanner) {
    expression(parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after expression");
    emitByte(parser, OP_POP);
}

static void ifStatement(Parser* parser, Scanner* scanner) {
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'");
    expression(parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition");

    int thenJump = emitJump(parser, OP_JUMP_IF_FALSE);
    emitByte(parser, OP_POP);
    statement(parser, scanner);

    // I think this messes up local variables, since they expect the stack to be empty.
    /* bool elseExists = match(parser, scanner, TOKEN_ELSE); */
    /*  */
    /* int elseJump = 0; */
    /* if (elseExists) { */
    /*     elseJump = emitJump(parser, OP_JUMP); */
    /* } */
    /*  */
    /* patchJump(parser, thenJump); */
    /* if (elseExists) { */
    /*     [> emitByte(parser, OP_POP); <] */
    /*     statement(parser, scanner); */
    /*     patchJump(parser, elseJump); */
    /* } */
    /* emitByte(parser, OP_POP); */

    // If I run into any weird bugs around "if" and the stack, just go back to this old code.
    int elseJump = emitJump(parser, OP_JUMP);
    
    patchJump(parser, thenJump);
    emitByte(parser, OP_POP);
    
    if(match(parser, scanner, TOKEN_ELSE)) statement(parser, scanner);
    patchJump(parser, elseJump);
}

static void whileStatement(Parser* parser, Scanner* scanner) {
    int loopStart = currentChunk()->count;
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'while'");
    expression(parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition");

    int exitJump = emitJump(parser, OP_JUMP_IF_FALSE);
    emitByte(parser, OP_POP);
    statement(parser, scanner);
    emitLoop(parser, loopStart);

    patchJump(parser, exitJump);
    emitByte(parser, OP_POP);
}

static void forStatement(Parser* parser, Scanner* scanner) {
    beginScope();
    consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'for'");
    if(match(parser, scanner, TOKEN_SEMICOLON)) {
        // no initializer
    } else if (match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(parser, scanner);
    } else {
        expressionStatement(parser, scanner);
    }

    int loopStart = currentChunk()->count;
   
    int exitJump = -1;
    if(!match(parser, scanner, TOKEN_SEMICOLON)) {
        expression(parser, scanner);
        consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after loop condition");

        exitJump = emitJump(parser, OP_JUMP_IF_FALSE);
        emitByte(parser, OP_POP); // remove condition
    }

    
    /* consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses"); */

    if(!match(parser, scanner, TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(parser, OP_JUMP);
        int incStart = currentChunk()->count;
        expression(parser, scanner);
        emitByte(parser, OP_POP);
        consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses");

        emitLoop(parser, loopStart);
        loopStart = incStart;
        patchJump(parser, bodyJump);
    }

    statement(parser, scanner);
    emitLoop(parser, loopStart);

    if(exitJump != -1) {
        patchJump(parser, exitJump);
        emitByte(parser, OP_POP);
    }

    endScope(parser);
}

static void statement(Parser* parser, Scanner* scanner) {
    if(match(parser, scanner, TOKEN_PRINT)) {
        printStatement(parser, scanner);
    } else if (match(parser, scanner, TOKEN_IF)) {
        ifStatement(parser, scanner);
    } else if (match(parser, scanner, TOKEN_RETURN)) {
        returnStatement(parser, scanner);
    } else if (match(parser, scanner, TOKEN_WHILE)) {
        whileStatement(parser, scanner);
    } else if (match(parser, scanner, TOKEN_FOR)) {
        forStatement(parser, scanner);
    } else if (match(parser, scanner, TOKEN_LEFT_BRACE)) {
        beginScope();
        block(parser, scanner);
        endScope(parser);
    } else {
        expressionStatement(parser, scanner);
    }
}

static void declaration(Parser* parser, Scanner* scanner) {
    if(match(parser, scanner, TOKEN_CLASS)) {
        classDeclaration(parser, scanner);
    } else if(match(parser, scanner, TOKEN_FUN)) {
        funDeclaration(parser, scanner);
    } else if(match(parser, scanner, TOKEN_VAR)) {
        varDeclaration(parser, scanner);
    } else {
        statement(parser, scanner);
    }

    if (parser->panicMode) synchronize(parser, scanner);
}


ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
    [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},

};

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

void Parser_init(Parser* parser) {
    parser->hadError = false;
    parser->panicMode = false;
}

ObjFunction* compile(const char* source) {
    Scanner scanner;
    Parser parser;
    Scanner_init(&scanner, source);
    Compiler compiler;
    Compiler_init(&compiler, &parser, TYPE_SCRIPT);
    /* compilingChunk = chunk; */

    advance(&parser, &scanner);
    /* expression(&parser, &scanner); */
    /* consume(&parser, &scanner, TOKEN_EOF, "Expect end of expression"); */
    while (!match(&parser, &scanner, TOKEN_EOF)) {
        declaration(&parser, &scanner);
    }

    ObjFunction* function = endCompiler(&parser);
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while(compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
