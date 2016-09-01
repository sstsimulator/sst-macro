#ifndef SSTMAC_MPI_INTEGERS_H
#define SSTMAC_MPI_INTEGERS_H

#include <sumi-mpi/mpi_types.h>

#ifdef __cplusplus
#include <cstdint>
#else
#include <stdint.h>
#endif

typedef uint32_t MPI_Request;
typedef long MPI_Op;

typedef uint16_t MPI_Datatype;
typedef long MPI_Comm;
#define MPI_COMM_WORLD ((MPI_Comm)0)
#define MPI_COMM_SELF  ((MPI_Comm)1)

typedef long MPI_Group;
#define MPI_GROUP_EMPTY ((MPI_Group)0x48000000)

/* for info */
typedef long MPI_Info;
#define MPI_INFO_NULL         ((MPI_Info)0x1c000000)
#define MPI_MAX_INFO_KEY       255
#define MPI_MAX_INFO_VAL      1024

/* RMA and Windows */
typedef int MPI_Win;
#define MPI_WIN_NULL ((MPI_Win)0x20000000)

/* Built in (0x1 in 30-31), errhandler (0x5 in bits 26-29, allkind (0
   in 22-25), index in the low bits */
typedef int MPI_Errhandler;
#define MPI_ERRORS_ARE_FATAL ((MPI_Errhandler)0x54000000)
#define MPI_ERRORS_RETURN    ((MPI_Errhandler)0x54000001)

//ops
enum _mpi_ops_ {
  MPI_MAX = 1,
  MPI_MIN = 2,
  MPI_SUM = 3,
  MPI_PROD = 4,
  MPI_LAND = 5,
  MPI_BAND = 6,
  MPI_LOR = 7,
  MPI_BOR = 8,
  MPI_LXOR = 9,
  MPI_BXOR = 10,
  MPI_MAXLOC = 11,
  MPI_MINLOC = 12,
  MPI_REPLACE = 13  //for MPI_Accumulate
};

// datatypes
enum _mpi_datatypes_ {
  MPI_NULL = 0,
  MPI_CHAR = 1,
  MPI_BYTE = 2,
  MPI_SHORT = 3,
  MPI_INT = 4,
  MPI_LONG = 5,
  MPI_FLOAT = 6,
  MPI_DOUBLE = 7,
  MPI_UNSIGNED_CHAR = 8,
  MPI_UNSIGNED_SHORT = 9,
  MPI_UNSIGNED = 10,
  MPI_UNSIGNED_LONG = 11,
  MPI_LONG_DOUBLE = 12,
  MPI_LONG_LONG_INT = 13,
  MPI_PACKED = 14,
  MPI_UB = 15,
  MPI_LB = 16,
  MPI_DOUBLE_INT = 17,
  MPI_2INT = 18,
  MPI_LONG_INT = 19,
  MPI_FLOAT_INT = 20,
  MPI_SHORT_INT = 21,
  MPI_LONG_DOUBLE_INT = 22,
  MPI_SIGNED_CHAR = 23,
  MPI_WCHAR = 24,
  MPI_UNSIGNED_LONG_LONG = 25,
  MPI_COMPLEX = 26,
  MPI_DOUBLE_COMPLEX = 27,
  MPI_LOGICAL = 28,
  MPI_REAL             ,
  MPI_DOUBLE_PRECISION ,
  MPI_INTEGER          ,
  MPI_2INTEGER         ,
  MPI_2REAL            ,
  MPI_2DOUBLE_PRECISION,
  MPI_CHARACTER,
 MPI_INT8_T,
MPI_INT16_T,
MPI_INT32_T,
MPI_INT64_T,
MPI_UINT8_T,
MPI_UINT16_T,
MPI_UINT32_T,
MPI_UINT64_T,
MPI_REAL4,
MPI_REAL8,
MPI_REAL16,
MPI_COMPLEX8,
MPI_COMPLEX16,
MPI_INTEGER1,
MPI_INTEGER2,
MPI_INTEGER4,
MPI_INTEGER8
};
#define MPI_LONG_LONG MPI_LONG_LONG_INT

#define MPI_IN_PLACE (void*)-1

/* Results of the compare operations. */
#define intENT     0
#define MPI_CONGRUENT 1
#define MPI_SIMILAR   2
#define MPI_UNEQUAL   3

/* Size-specific types (see MPI-2, 10.2.5) */


/* C99 fixed-width datatypes */


/* These are only guesses; make sure you change them in mpif.h as well */
#define MPI_MAX_PROCESSOR_NAME 128
#define MPI_MAX_ERROR_STRING   1024
#define MPI_MAX_PORT_NAME      256
#define MPI_MAX_OBJECT_NAME    128

/* Define some null objects */
#define MPI_COMM_NULL      ((MPI_Comm)0x04000000)
#define MPI_OP_NULL        ((MPI_Op)0x18000000)
#define MPI_GROUP_NULL     ((MPI_Group)0x08000000)
#define MPI_DATATYPE_NULL  ((MPI_Datatype)0x0c000000)
#define MPI_REQUEST_NULL   ((MPI_Request)0x2c000000)
#define MPI_GROUP_WORLD   0

#define MPI_ADDRESS_KIND  4

/*  for rma windows  */
#define MPI_WIN_BASE        64
#define MPI_WIN_SIZE        128
#define MPI_WIN_DISP_UNIT   256


/* for subarray and darray constructors */
#define MPI_ORDER_C              56
#define MPI_ORDER_FORTRAN        57
#define MPI_DISTRIBUTE_BLOCK    121
#define MPI_DISTRIBUTE_CYCLIC   122
#define MPI_DISTRIBUTE_NONE     123
#define MPI_DISTRIBUTE_DFLT_DARG -49767

/* asserts for one-sided communication */
#define MPI_MODE_NOCHECK      1024
#define MPI_MODE_NOSTORE      2048
#define MPI_MODE_NOPUT        4096
#define MPI_MODE_NOPRECEDE    8192
#define MPI_MODE_NOSUCCEED   16384

/* Pre-defined constants */
#define MPI_UNDEFINED      (-32766)
#define MPI_KEYVAL_INVALID 0x24000000

typedef int (MPI_Copy_function)(MPI_Comm, int, void*, void*, void*, int*);
typedef int (MPI_Delete_function)(MPI_Comm, int, void*, void*);

#endif // SSTMAC_MPI_INTEGERS_H
