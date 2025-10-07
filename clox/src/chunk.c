#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"

void Chunk_init(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    ValueArray_init(&chunk->constants);
}

void Chunk_free(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    ValueArray_free(&chunk->constants);
    Chunk_init(chunk);
}

void Chunk_write(Chunk* chunk, uint8_t byte, int line) {
    if(chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

int Chunk_addConstant(Chunk* chunk, Value value) {
    VM_push(value);
    ValueArray_write(&chunk->constants, value);
    VM_pop();
    return chunk->constants.count - 1;
}
