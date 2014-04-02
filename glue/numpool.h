#ifndef INTPOOL_H
#define INTPOOL_H

#include <stdlib.h>
#include <stdint.h>

typedef struct {
    void* pool;
    size_t size;
    void* fill;
} numpool_t;

numpool_t *numpool_create();
void numpool_reset(numpool_t * pool);
void numpool_free(numpool_t * pool);

uint8_t *numpool_uint8(numpool_t * pool, uint8_t i);
int8_t *numpool_int8(numpool_t * pool, int8_t i);
uint16_t *numpool_uint16(numpool_t * pool, uint16_t i);
int16_t *numpool_int16(numpool_t * pool, int16_t i);
uint32_t *numpool_uint32(numpool_t * pool, uint32_t i);
int32_t *numpool_int32(numpool_t * pool, int32_t i);
uint64_t *numpool_uint64(numpool_t * pool, uint64_t i);
int64_t *numpool_int64(numpool_t * pool, int64_t i);
float *numpool_float(numpool_t * pool, float i);
double *numpool_double(numpool_t * pool, double i);

#endif
