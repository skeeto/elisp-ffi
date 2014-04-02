#ifndef STACK_H
#define STACK_H

#include <stdlib.h>
#include <inttypes.h>
#include <ffi.h>

typedef struct {
    ffi_type **types;
    void **values;
    size_t size;
    size_t count;
} stack_t;

stack_t *stack_create();
void stack_push(stack_t *stack, ffi_type *type, void *value);
ffi_type *stack_peek_type(stack_t *stack);
void *stack_peek_value(stack_t *stack);
void *stack_pop(stack_t *stack);
void stack_free(stack_t *stack);

#endif
