#ifndef SSTMAC_MPI_INTEGERS_H
#define SSTMAC_MPI_INTEGERS_H

#include <sumi-mpi/mpi_types.h>

typedef long MPI_Request;
typedef long MPI_Op;

typedef long MPI_Datatype;
typedef long MPI_Comm;
#define MPI_COMM_WORLD ((MPI_Comm)0x44000000)
#define MPI_COMM_SELF  ((MPI_Comm)0x44000001)

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
  MPI_MIN,
  MPI_SUM,
  MPI_PROD,
  MPI_LAND,
  MPI_BAND,
  MPI_LOR,
  MPI_BOR,
  MPI_LXOR,
  MPI_BXOR,
  MPI_MAXLOC,
  MPI_MINLOC,
  MPI_REPLACE  //for MPI_Accumulate
};

// datatypes
enum _mpi_datatypes_ {
  MPI_NULL = 0,
  MPI_CHAR,
  MPI_BYTE,
  MPI_SHORT,
  MPI_INT,
  MPI_LONG,
  MPI_FLOAT,
  MPI_DOUBLE,
  MPI_UNSIGNED_CHAR,
  MPI_UNSIGNED_SHORT,
  MPI_UNSIGNED,
  MPI_UNSIGNED_LONG,
  MPI_LONG_DOUBLE,
  MPI_LONG_LONG_INT,
  MPI_PACKED,
  MPI_UB,
  MPI_LB,
  MPI_DOUBLE_INT,
  MPI_2INT,
  MPI_LONG_INT,
  MPI_FLOAT_INT,
  MPI_SHORT_INT,
  MPI_LONG_DOUBLE_INT,
  MPI_SIGNED_CHAR   ,
  MPI_WCHAR         ,
  MPI_UNSIGNED_LONG_LONG,
  MPI_COMPLEX          ,
  MPI_DOUBLE_COMPLEX   ,
  MPI_LOGICAL          ,
  MPI_REAL             ,
  MPI_DOUBLE_PRECISION ,
  MPI_INTEGER          ,
  MPI_2INTEGER         ,
  MPI_2REAL            ,
  MPI_2DOUBLE_PRECISION,
  MPI_CHARACTER
};
#define MPI_LONG_LONG MPI_LONG_LONG_INT

#define MPI_IN_PLACE (void*)-1

/* Results of the compare operations. */
#define intENT     0
#define MPI_CONGRUENT 1
#define MPI_SIMILAR   2
#define MPI_UNEQUAL   3

/* Size-specific types (see MPI-2, 10.2.5) */
#define MPI_REAL4             ((MPI_Datatype)0x4c000427)
#define MPI_REAL8             ((MPI_Datatype)0x4c000829)
#define MPI_REAL16            ((MPI_Datatype)0x4c00102b)
#define MPI_COMPLEX8          ((MPI_Datatype)0x4c000828)
#define MPI_COMPLEX16         ((MPI_Datatype)0x4c00102a)
#define MPI_COMPLEX32         ((MPI_Datatype)0x4c00202c)
#define MPI_INTEGER1          ((MPI_Datatype)0x4c00012d)
#define MPI_INTEGER2          ((MPI_Datatype)0x4c00022f)
#define MPI_INTEGER4          ((MPI_Datatype)0x4c000430)
#define MPI_INTEGER8          ((MPI_Datatype)0x4c000831)
#define MPI_INTEGER16         ((MPI_Datatype)MPI_DATATYPE_NULL)

/* C99 fixed-width datatypes */
#define MPI_INT8_T            ((MPI_Datatype)0x4c000137)
#define MPI_INT16_T           ((MPI_Datatype)0x4c000238)
#define MPI_INT32_T           ((MPI_Datatype)0x4c000439)
#define MPI_INT64_T           ((MPI_Datatype)0x4c00083a)
#define MPI_UINT8_T           ((MPI_Datatype)0x4c00013b)
#define MPI_UINT16_T          ((MPI_Datatype)0x4c00023c)
#define MPI_UINT32_T          ((MPI_Datatype)0x4c00043d)
#define MPI_UINT64_T          ((MPI_Datatype)0x4c00083e)

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
