#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <ffi.h>
#include <dlfcn.h>

#include "stack.h"
#include "numpool.h"

#define typeprint(type, upper)                                  \
    printf("(%s \"%p\" %d)", #type, &ffi_type_##type, upper)

void print_sizes()
{
    printf("(");
    typeprint(void, FFI_TYPE_VOID);
    typeprint(uint8, FFI_TYPE_UINT8);
    typeprint(sint8, FFI_TYPE_SINT8);
    typeprint(uint16, FFI_TYPE_UINT16);
    typeprint(sint16, FFI_TYPE_SINT16);
    typeprint(uint32, FFI_TYPE_UINT32);
    typeprint(sint32, FFI_TYPE_SINT32);
    typeprint(uint64, FFI_TYPE_UINT64);
    typeprint(sint64, FFI_TYPE_SINT64);
    typeprint(float, FFI_TYPE_FLOAT);
    typeprint(double, FFI_TYPE_DOUBLE);
    typeprint(uchar, FFI_TYPE_UINT8);
    typeprint(schar, FFI_TYPE_SINT8);
    typeprint(ushort, FFI_TYPE_UINT16);
    typeprint(sshort, FFI_TYPE_SINT16);
    typeprint(uint, FFI_TYPE_UINT32);
    typeprint(sint, FFI_TYPE_SINT32);
    typeprint(ulong,
              sizeof(long) == 4 ? FFI_TYPE_UINT32 : FFI_TYPE_UINT64);
    typeprint(slong,
              sizeof(long) == 4 ? FFI_TYPE_UINT32 : FFI_TYPE_UINT64);
    typeprint(pointer, FFI_TYPE_POINTER);
    printf(")\n");
}

void *read_binary(size_t size) {
    size_t i;
    char *string = malloc(size + 1);
    for (i = 0; i < size; i++) {
        string[i] = getchar();
    }
    string[size] = '\0';
    return string;
}

void print_value(ffi_type *type, void *value) {
    if (type == &ffi_type_uint8) {
        printf("%" PRIu8, *((uint8_t *) value));
    } else if (type == &ffi_type_uint16) {
        printf("%" PRIu16, *((uint16_t *) value));
    } else if (type == &ffi_type_uint32) {
        printf("%" PRIu32, *((uint32_t *) value));
    } else if (type == &ffi_type_uint64) {
        printf("%" PRIu64, *((uint64_t *) value));
    } else if (type == &ffi_type_sint8) {
        printf("%" PRId8, *((int8_t *) value));
    } else if (type == &ffi_type_sint16) {
        printf("%" PRId16, *((int16_t *) value));
    } else if (type == &ffi_type_uint32) {
        printf("%" PRId32, *((int32_t *) value));
    } else if (type == &ffi_type_uint64) {
        printf("%" PRId64, *((int64_t *) value));
    } else if (type == &ffi_type_float) {
        printf("%f", *((float *) value));
    } else if (type == &ffi_type_double) {
        printf("%f", *((double *) value));
    } else if (type == &ffi_type_pointer) {
        printf("\"%p\"", value);
    }
}

int main()
{
    int8_t *i8;
    int16_t *i16;
    int32_t *i32;
    int64_t *i64;
    uint8_t *u8;
    uint16_t *u16;
    uint32_t *u32;
    uint64_t *u64;
    float *f32;
    double *f64;
    void *value, *null = NULL;

    char *lib, *symbol;

    ffi_cif cif;

    char *arg = "HOME";
    void *args[1];
    args[0] = &arg;

    stack_t *stack = stack_create();
    numpool_t *pool = numpool_create();
    while (!feof(stdin)) {
        char command = getchar();
        switch (command) {
        case 'I':
            /* info */
            print_sizes();
            break;

            /* push signed */
        case 'i' + 0: /* i */
            i8 = numpool_int8(pool, 0);
            scanf("%" SCNd8, i8);
            stack_push(stack, &ffi_type_sint8, i8);
            break;
        case 'i' + 1: /* j */
            i16 = numpool_int16(pool, 0);
            scanf("%" SCNd16, i16);
            stack_push(stack, &ffi_type_sint16, i16);
            break;
        case 'i' + 2: /* k */
            i32 = numpool_int32(pool, 0);
            scanf("%" SCNd32, i32);
            stack_push(stack, &ffi_type_sint32, i32);
            break;
        case 'i' + 3: /* l */
            i64 = numpool_int64(pool, 0);
            scanf("%" SCNd64, i64);
            stack_push(stack, &ffi_type_sint64, i64);
            break;

            /* push unsigned */
        case 'u' + 0: /* u */
            u8 = numpool_uint8(pool, 0);
            scanf("%" SCNu8, u8);
            stack_push(stack, &ffi_type_uint8, u8);
            break;
        case 'u' + 1: /* v */
            u16 = numpool_uint16(pool, 0);
            scanf("%" SCNu16, u16);
            stack_push(stack, &ffi_type_uint16, u16);
            break;
        case 'u' + 2: /* w */
            u32 = numpool_uint32(pool, 0);
            scanf("%" SCNu32, u32);
            stack_push(stack, &ffi_type_uint32, u32);
            break;
        case 'u' + 3: /* x */
            u64 = numpool_uint64(pool, 0);
            scanf("%" SCNu64, u64);
            stack_push(stack, &ffi_type_uint64, u64);
            break;

            /* push float */
        case 'f':
            f32 = numpool_float(pool, 0);
            scanf("%f", f32);
            stack_push(stack, &ffi_type_float, f32);
            break;
        case 'd':
            f64 = numpool_double(pool, 0);
            scanf("%lf", f64);
            stack_push(stack, &ffi_type_double, f64);
            break;

        case 'p':
            /* push pointer */
            scanf("%" SCNxPTR " ", (long *) &value);
            stack_push(stack, &ffi_type_pointer, value);
            break;

        case 'e':
            /* peek */
            print_value(stack_peek_type(stack), stack_peek_value(stack));
            break;
        case 'o':
            /* pop */
            print_value(stack_peek_type(stack), stack_peek_value(stack));
            stack_pop(stack);
            break;
        case 'c':
            /* call */
            value = stack_pop(stack); /* function */
            u32 = stack_pop(stack);       /* nargs */
            ffi_prep_cif(&cif, FFI_DEFAULT_ABI, *u32,
                         stack->types[stack->count - *u32 - 1],
                         stack->types + stack->count - *u32);
            args[0] = stack->values + stack->count - *u32;
            ffi_call(&cif, value,
                     &stack->values[stack->count - *u32 - 1],
                     args);
            //stack->values + stack->count - *u32);
            stack->count -= *u32;
            break;
        case 'M':
            /* malloc */
            u32 = stack_pop(stack);
            stack_push(stack, &ffi_type_pointer, read_binary(*u32));
            break;
        case 'F':
            /* free */
            free(stack_pop(stack));
            break;
        case 'N':
            /* push NULL */
            stack_push(stack, &ffi_type_pointer, null);
            break;
        case 'P':
            /* get function pointer. */
            lib = stack_pop(stack);
            symbol = stack_pop(stack);
            value = dlsym(dlopen(lib, RTLD_LAZY), symbol);
            stack_push(stack, &ffi_type_pointer, value);
            break;
        case 'S':
            /* print string */
            printf("\"%s\"", (char *) stack_pop(stack));
            break;
        }
        fflush(stdout);
        if (stack->count == 0) {
            numpool_reset(pool);
        }
    }

    printf("\n");
    int i;
    for(i = stack->count - 1; i >= 0 ; i--){
        printf("%d> ", i);
        print_value(stack->types[i], stack->values[i]);
        printf("\n");
    }

    numpool_free(pool);
    stack_free(stack);

    printf("\n");
    return EXIT_SUCCESS;
}
