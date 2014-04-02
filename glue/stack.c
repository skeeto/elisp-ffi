#include "stack.h"

stack_t *stack_create()
{
    stack_t *stack = malloc(sizeof(stack_t));
    stack->size = 16;
    stack->count = 0;
    stack->types = malloc(stack->size * sizeof(ffi_type *));
    stack->values = malloc(stack->size * sizeof(void *));
    return stack;
}

void stack_grow(stack_t * stack)
{
    stack->size *= 2;
    stack->types = realloc(stack->types, sizeof(ffi_type *) * stack->size);
    stack->values = realloc(stack->values, sizeof(void *) * stack->size);
}

void stack_push(stack_t * stack, ffi_type *type, void *value)
{
    if (stack->count >= stack->size) {
        stack_grow(stack);
    }
    stack->types[stack->count] = type;
    stack->values[stack->count] = value;
    stack->count++;
}

ffi_type *stack_peek_type(stack_t *stack) {
    return stack->types[stack->count - 1];
}

void *stack_peek_value(stack_t *stack) {
    return stack->values[stack->count - 1];
}

void *stack_pop(stack_t * stack)
{
    stack->count--;
    return stack->values[stack->count];
}

void stack_free(stack_t * stack)
{
    free(stack->types);
    free(stack->values);
    free(stack);
}
