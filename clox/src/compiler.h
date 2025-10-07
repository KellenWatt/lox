#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "chunk.h"
#include "scanner.h"

ObjFunction* compile(const char* source);

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

void Parser_init(Parser* parser);

void markCompilerRoots();

#endif
