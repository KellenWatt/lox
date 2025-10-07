#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

typedef struct {
    ObjString* key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void Table_init(Table* table);
void Table_free(Table* table);

bool Table_set(Table* table, ObjString* key, Value value);
bool Table_get(Table* table, ObjString* key, Value* value);
bool Table_delete(Table* table, ObjString* key);
void Table_addAll(Table* from, Table* to);

ObjString* Table_findString(Table* table, const char* chars, int length, uint32_t hash);

void tableRemoveWhite(Table* table);
void markTable(Table* table);
#endif
