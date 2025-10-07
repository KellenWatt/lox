#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

// #define STACK_MAX 256
#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;
    Value stack[STACK_MAX];
    Value* stackTop;
    Table strings;
    Table globals;
    ObjString* initString;
    ObjUpvalue* openUpvalues;
    Obj* objects;
    
    size_t bytesAllocated;
    size_t nextGC;

    int grayCount;
    int grayCapacity;
    Obj** grayStack;
} VM;

extern VM vm;

void VM_init();
void VM_free();

void VM_push(Value value);
Value VM_pop();

InterpretResult VM_interpret(const char* source);

#endif
