#ifndef MPITYPES_H_IN
#define MPITYPES_H_IN

#ifdef __cpluscplus
#include <cstdint>
#else
#include <stdint.h>
#endif

typedef uintptr_t MPI_Aint;

// useful flags
#define MPI_PROC_NULL   (-1)
#define MPI_ANY_SOURCE  (-2)
#define MPI_ROOT        (-3)
#define MPI_ANY_TAG     (-1)

#endif // MPITYPES_H_IN
