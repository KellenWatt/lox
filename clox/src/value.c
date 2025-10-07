#include <stdio.h>
#include <memory.h>

#include "memory.h"
#include "value.h"

void Value_print(Value value) {
    switch(value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL: printf("nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(value); break;
    }
}

bool Value_equals(Value a, Value b) {
    if(a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL:   return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:    return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:    return AS_OBJ(a) == AS_OBJ(b);
        default:         return false; // unreachable
    }
}


void ValueArray_init(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void ValueArray_write(ValueArray* array, Value value) {
    if(array->capacity < array->count + 1) {
        int oldCap = array->capacity;
        array->capacity = GROW_CAPACITY(oldCap);
        array->values = GROW_ARRAY(Value, array->values, oldCap, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void ValueArray_free(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    ValueArray_init(array);
}
