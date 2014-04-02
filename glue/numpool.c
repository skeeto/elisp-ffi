#include <stddef.h>
#include "numpool.h"

numpool_t *numpool_create()
{
    numpool_t *pool = malloc(sizeof(numpool_t));
    pool->size = 4096;
    pool->pool = malloc(pool->size);
    pool->fill = pool->pool;
    return pool;
}

void numpool_reset(numpool_t * pool)
{
    pool->fill = pool->pool;
}

void numpool_free(numpool_t * pool)
{
    free(pool->pool);
    free(pool);
}

void numpool_grow(numpool_t * pool)
{
    pool->size *= 2;
    ptrdiff_t diff = pool->fill - pool->pool;
    pool->pool = realloc(pool->pool, pool->size);
    pool->fill = pool->pool + diff;
}

#define NUMPOOL(type, name) \
    type *numpool_##name(numpool_t * pool, type i) { \
    if (pool->fill + sizeof(type) > pool->pool + pool->size) { \
        numpool_grow(pool); \
    } \
    type *pi = pool->fill; \
    *pi = i; \
    pool->fill += sizeof(type); \
    return pi; \
}


NUMPOOL(int8_t,  int8);
NUMPOOL(int16_t, int16);
NUMPOOL(int32_t, int32);
NUMPOOL(int64_t, int64);

NUMPOOL(uint8_t,  uint8);
NUMPOOL(uint16_t, uint16);
NUMPOOL(uint32_t, uint32);
NUMPOOL(uint64_t, uint64);

NUMPOOL(float, float);
NUMPOOL(double, double);
