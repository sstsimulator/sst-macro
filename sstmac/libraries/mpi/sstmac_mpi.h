/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_SSTMPI_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_SSTMPI_H_INCLUDED


#include <sstmac/libraries/mpi/sstmac_mpi_macro.h>
#include <sstmac/libraries/mpi/sstmac_mpi_integers.h>


#define MPI_LOCK_SHARED 0
#define MPI_LOCK_EXCLUSIVE 1

/** Define the error codes for use as return values */
#define   MPI_SUCCESS         0
#define   MPI_ERR_BUFFER      1  // Invalid buffer pointer
#define   MPI_ERR_COUNT       2  // Invalid count argument
#define   MPI_ERR_TYPE        3  // Invalid datatype argument
#define   MPI_ERR_TAG         4  // Invalid tag argument
#define   MPI_ERR_COMM        5  // Invalid communicator
#define   MPI_ERR_RANK        6  // Invalid rank
#define   MPI_ERR_REQUEST     7  // Invalid request
#define   MPI_ERR_ROOT        8  // Invalid root
#define   MPI_ERR_GROUP       9  // Invalid group
#define   MPI_ERR_OP          10 // Invalid operation
#define   MPI_ERR_TOPOLOGY    11 // Invalid topology
#define   MPI_ERR_DIMS        12 // Invalid dimensions argument
#define   MPI_ERR_ARG         13 // Invalid argument of some other kind
#define   MPI_ERR_UNKNOWN     14 // Unknown error
#define   MPI_ERR_TRUNCATE    15 // Message truncated on receive
#define   MPI_ERR_OTHER       16 // Known error not in this list
#define   MPI_ERR_INTERN      17 // Internal MPI error
#define   MPI_ERR_IN_STATUS   18 // Error code is in status
#define   MPI_ERR_PENDING     19 // Pending request
#define   MPI_ERR_NO_MEM      20
#define   MPI_ERR_LASTCODE    21 // Last error code
/** Define a null payload */
#define MPI_PAYLOAD_IGNORE NULL
#define MPI_STATUSES_IGNORE 0
#define MPI_STATUS_IGNORE 0


/* Make the C names for the dup function mixed case.
   This is required for systems that use all uppercase names for Fortran
   externals.  */
/* MPI 1 names */
#define MPI_NULL_COPY_FN   ((MPI_Copy_function *)0)
#define MPI_NULL_DELETE_FN ((MPI_Delete_function *)0)
#define MPI_DUP_FN         MPIR_Dup_fn
/* MPI 2 names */
#define MPI_COMM_NULL_COPY_FN ((MPI_Comm_copy_attr_function*)0)
#define MPI_COMM_NULL_DELETE_FN ((MPI_Comm_delete_attr_function*)0)
#define MPI_COMM_DUP_FN  ((MPI_Comm_copy_attr_function *)MPI_DUP_FN)
#define MPI_WIN_NULL_COPY_FN ((MPI_Win_copy_attr_function*)0)
#define MPI_WIN_NULL_DELETE_FN ((MPI_Win_delete_attr_function*)0)
#define MPI_WIN_DUP_FN   ((MPI_Win_copy_attr_function*)MPI_DUP_FN)
#define MPI_TYPE_NULL_COPY_FN ((MPI_Type_copy_attr_function*)0)
#define MPI_TYPE_NULL_DELETE_FN ((MPI_Type_delete_attr_function*)0)
#define MPI_TYPE_DUP_FN ((MPI_Type_copy_attr_function*)MPI_DUP_FN)

/* Topology types */
typedef enum MPIR_Topo_type { MPI_GRAPH=1, MPI_CART=2, MPI_DIST_GRAPH=3 } MPIR_Topo_type;

#define MPI_BOTTOM      (void *)0
extern int * const MPI_UNWEIGHTED;


/* Permanent key values */
/* C Versions (return pointer to value),
   Fortran Versions (return integer value).
   Handled directly by the attribute value routine

   DO NOT CHANGE THESE.  The values encode:
   builtin kind (0x1 in bit 30-31)
   Keyval object (0x9 in bits 26-29)
   for communicator (0x1 in bits 22-25)

   Fortran versions of the attributes are formed by adding one to
   the C version.
 */
#define MPI_TAG_UB           0x64400001
#define MPI_HOST             0x64400003
#define MPI_IO               0x64400005
#define MPI_WTIME_IS_GLOBAL  0x64400007
#define MPI_UNIVERSE_SIZE    0x64400009
#define MPI_LASTUSEDCODE     0x6440000b
#define MPI_APPNUM           0x6440000d

typedef struct {
  long MPI_SOURCE;
  long MPI_TAG;
  long MPI_ERROR;
  long BYTES_RECV;
  long COUNT;    //this is in bytes, yo!
  int CANCELLED;
} MPI_Status;

typedef int MPI_Fint;
typedef struct ADIOI_FileD *MPI_File;
#define MPI_FILE_NULL NULL

#define MPI_VERSION    2
#define MPI_SUBVERSION 0

/* For supported thread levels */
#define MPI_THREAD_SINGLE 0
#define MPI_THREAD_FUNNELED 1
#define MPI_THREAD_SERIALIZED 2
#define MPI_THREAD_MULTIPLE 3

/* Upper bound on the overhead in bsend for each message buffer */
#define MPI_BSEND_OVERHEAD 88

/* User combination function */
typedef void (MPI_User_function) ( void *, void *, int *, MPI_Datatype * );

/* C functions */
typedef void (MPI_Handler_function) ( MPI_Comm *, int *, ... );
typedef int (MPI_Comm_copy_attr_function)(MPI_Comm, int, void *, void *,
    void *, int *);
typedef int (MPI_Comm_delete_attr_function)(MPI_Comm, int, void *, void *);
typedef int (MPI_Type_copy_attr_function)(MPI_Datatype, int, void *, void *,
    void *, int *);
typedef int (MPI_Type_delete_attr_function)(MPI_Datatype, int, void *, void *);
typedef int (MPI_Win_copy_attr_function)(MPI_Win, int, void *, void *, void *,
    int *);
typedef int (MPI_Win_delete_attr_function)(MPI_Win, int, void *, void *);
/* added in MPI-2.2 */
typedef void (MPI_Comm_errhandler_function)(MPI_Comm *, int *, ...);
typedef void (MPI_File_errhandler_function)(MPI_File *, int *, ...);
typedef void (MPI_Win_errhandler_function)(MPI_Win *, int *, ...);
/* names that were added in MPI-2.0 and deprecated in MPI-2.2 */
typedef MPI_Comm_errhandler_function MPI_Comm_errhandler_fn;
typedef MPI_File_errhandler_function MPI_File_errhandler_fn;
typedef MPI_Win_errhandler_function MPI_Win_errhandler_fn;

/* Typedefs for generalized requests */
typedef int (MPI_Grequest_cancel_function)(void *, int);
typedef int (MPI_Grequest_free_function)(void *);
typedef int (MPI_Grequest_query_function)(void *, MPI_Status *);


#define MPI_TYPECLASS_REAL 1
#define MPI_TYPECLASS_INTEGER 2
#define MPI_TYPECLASS_COMPLEX 3

#ifdef __cplusplus
extern "C"
{
#endif

long
MPI_NodeAddress(int rank, MPI_Comm c);

long
MPI_Taskid(int rank, MPI_Comm c);

int
MPI_Errhandler_set(MPI_Comm c, int err);

void
MPI_Print(const char* msg);

int
MPI_Init(int *, char ***);

int
MPI_Initialized(int *);

void
MPI_Abort(MPI_Comm c, int code);

int
MPI_Finalize(void);

int
MPI_Wait(MPI_Request *request, MPI_Status *status);

int
MPI_Waitall(int count, MPI_Request array_of_requests[],
            MPI_Status *status);

int
MPI_Waitany(int count, MPI_Request array_of_requests[], int *index,
            MPI_Status *status);

int
MPI_Waitany_timeout(int count, MPI_Request array_of_requests[], int *index,
                    MPI_Status *status, double timeout);

int
MPI_Waitsome(int incount, MPI_Request array_of_requests[], int *outcount,
             int array_of_indices[], MPI_Status array_of_statuses[]);

int
MPI_Get_count(MPI_Status *status, MPI_Datatype datatype, int *count);

int
MPI_Test(MPI_Request *request, int *flag, MPI_Status *status);

int
MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status);

/**
 * MPI_Cancel also frees the request, and it should not be used again.
 * @param request
 * @return
 */
int
MPI_Cancel(MPI_Request *request);

int
MPI_Request_free(MPI_Request *request);

int
MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm);
int
MPI_Comm_rank(MPI_Comm comm, int *rank);
int
MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);
int
MPI_Comm_size(MPI_Comm comm, int *size);
int
MPI_Comm_free(MPI_Comm *comm);
int
MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm);
int
MPI_Comm_group(MPI_Comm comm, MPI_Group *group);

int
MPI_Group_incl(MPI_Group group, int n, int *ranks, MPI_Group *newgroup);
int
MPI_Group_free(MPI_Group *group);

int
MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
         MPI_Comm comm);

int
MPI_Bsend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
          MPI_Comm comm);

int
MPI_Rsend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
          MPI_Comm comm);

int
MPI_Ssend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
          MPI_Comm comm);

int
MPI_Sendrecv(void *sendbuf, int sendcount, MPI_Datatype sendtype, int dest,
             int sendtag, void *recvbuf, int recvcount, MPI_Datatype recvtype,
             int source, int recvtag, MPI_Comm comm, MPI_Status* status);

int
MPI_Isend(void *buf, int count, MPI_Datatype datatype, int dest,
          int tag, MPI_Comm comm, MPI_Request *request);

int
MPI_Ibsend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
           MPI_Comm comm, MPI_Request *request);

int
MPI_Irsend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
           MPI_Comm comm, MPI_Request *request);

int
MPI_Issend(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
           MPI_Comm comm, MPI_Request *request);

int
MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
         MPI_Comm comm, MPI_Status *status);

/**
 * Irecv  -- MPI_Recover_Payload must be used to set the payload buffer
 * @param buf Not used, pass NULL
 */
int
MPI_Irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
          MPI_Comm comm, MPI_Request *request);

/**
 * --- MPI_Recover_Payload - used to recover the payload on an Irecv
 * @param buf
 * @param request
 * @return 0 if successful, 1 if request is not complete yet.
 */
int
MPI_Recover_Payload(void *buf, MPI_Request request);

int
MPI_Allreduce(void *sendbuf, void *recvbuf, int count,
              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);

int
MPI_Reduce(void *sendbuf, void *recvbuf, int count,
           MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm);

int
MPI_Barrier(MPI_Comm comm);

int
MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root,
          MPI_Comm comm);

int
MPI_Scan(void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
         MPI_Op op, MPI_Comm comm);

int
MPI_Gather(void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf,
           int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm);

int
MPI_Gatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
            void *recvbuf, int *recvcnts, int *displs, MPI_Datatype recvtype,
            int root, MPI_Comm comm);

int
MPI_Allgather(void *sendbuf, int sendcount, MPI_Datatype sendtype,
              void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm);

int
MPI_Allgatherv(void *sendbuf, int sendcount, MPI_Datatype sendtype,
               void *recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype,
               MPI_Comm comm);

int
MPI_Scatter(void *sendbuf, int sendcnt, MPI_Datatype sendtype, void *recvbuf,
            int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm);

int
MPI_Scatterv(void *sendbuf, int *sendcnts, int *displs,
             MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype,
             int root, MPI_Comm comm);

int
MPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype,
             void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm);

int
MPI_Alltoallv(void *sendbuf, int *sendcnts, int *sdispls,
              MPI_Datatype sendtype, void *recvbuf, int *recvcnts,
              int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);

double
MPI_Wtime();

int
MPI_Send_init(void *buf, int count, MPI_Datatype datatype, int dest,
              int tag, MPI_Comm comm, MPI_Request *request);

int
MPI_Bsend_init(void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm, MPI_Request *request);

int
MPI_Rsend_init(void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm, MPI_Request *request);

int
MPI_Ssend_init(void *buf, int count, MPI_Datatype datatype, int dest,
               int tag, MPI_Comm comm, MPI_Request *request);

int
MPI_Recv_init(void *buf, int count, MPI_Datatype datatype, int source,
              int tag, MPI_Comm comm, MPI_Request *request);

int
MPI_Startall(int count, MPI_Request array_of_requests[]);


int
MPI_Start(MPI_Request *request);

int
MPI_Testall(int count, MPI_Request array_of_requests[], int *flag,
            MPI_Status array_of_statuses[]);





//----------------------------------------------------------------
// --- MPI Cart functions
//----------------------------------------------------------------
int
MPI_Cart_create(MPI_Comm comm, int ndims, int *dims, int *periods,
                int reorder, MPI_Comm *outcomm);

int
MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims, int *periods,
             int *coords);

int
MPI_Cart_rank(MPI_Comm comm, int *coords, int *rank);

int
MPI_Cart_shift(MPI_Comm comm, int direction, int displ, int *source,
               int *dest);

int
MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int *coords);

int
MPI_Address(void *location, MPI_Aint *address);

int
MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status);

int
MPI_Cartdim_get(MPI_Comm comm, int *ndims);

double
MPI_Wtick();


//----------------------------------------------------------------
// --- MPI Derived Datatype (experimental)
//----------------------------------------------------------------

int
MPI_Type_contiguous(int count, MPI_Datatype old_type,
                    MPI_Datatype *new_type_p);

int
MPI_Type_vector(int count, int blocklength, int stride,
                MPI_Datatype old_type, MPI_Datatype *newtype_p);

int
MPI_Type_hvector(int count, int blocklength, MPI_Aint stride,
                 MPI_Datatype old_type, MPI_Datatype *newtype_p);

int
MPI_Type_struct(int count, int blocklens[], MPI_Aint indices[],
                MPI_Datatype old_types[], MPI_Datatype *newtype);

int
MPI_Type_commit(MPI_Datatype *datatype);

int
MPI_Type_free(MPI_Datatype *datatype);

//----------------------------------------------------------------
// --- Functions to control SST/macro behavior
//----------------------------------------------------------------

void
MPI_Disable_Payloads();

//void
//MPI_Payloads_disable() { MPI_Disable_Payloads(); }

void
MPI_Enable_Payloads();

//void
//MPI_Payloads_enable() { MPI_Enable_Payloads(); }


//----------------------------------------------------------------
// --- Misc functions added to support parmetis and libtopomap
//----------------------------------------------------------------


int
MPI_Keyval_create(MPI_Copy_function *copy_fn,
                  MPI_Delete_function *delete_fn,
                  int *keyval, void *extra_state);

int
MPI_Attr_put(MPI_Comm comm, int keyval, void *attr_value);

int
MPI_Attr_get(MPI_Comm comm, int keyval, void *attr_value, int *flag);

int
MPI_Reduce_scatter(void *sendbuf, void *recvbuf, int *recvcnts,
                   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);

MPI_Comm MPI_Comm_f2c(MPI_Fint comm);
MPI_Fint MPI_Comm_c2f(MPI_Comm comm);

int
MPI_Topo_test( MPI_Comm comm, int *topo_type);

int
MPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors,
                    int *neighbors);

int
MPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors);

int
MPI_Group_translate_ranks(MPI_Group group1, int n, int *ranks1,
                          MPI_Group group2, int *ranks2);


int MPI_Group_range_incl(MPI_Group, int, int [][3], MPI_Group *);
int MPI_Keyval_free(int *);
int MPI_Comm_compare(MPI_Comm, MPI_Comm, int *);
int MPI_Type_extent(MPI_Datatype, MPI_Aint *);
int MPI_Type_dup(MPI_Datatype, MPI_Datatype *);
int MPI_Type_set_name(MPI_Datatype, char *);
int MPI_Type_indexed(int, int *, int *, MPI_Datatype, MPI_Datatype *);
int MPI_Type_size(MPI_Datatype, int *);
int MPI_Type_get_name(MPI_Datatype, char *, int *);
int MPI_Intercomm_create(MPI_Comm, int, MPI_Comm, int, int, MPI_Comm * );
int MPI_Group_excl(MPI_Group, int, int *, MPI_Group *);
int MPI_Comm_remote_size(MPI_Comm, int *);
int MPI_Error_class(int, int *);
int MPI_Error_string(int, char *, int *);
int MPI_Buffer_attach( void*, int);
int MPI_Buffer_detach( void*, int *);
int MPI_Testany(int, MPI_Request *, int *, int *, MPI_Status *);
int MPI_Testsome(int, MPI_Request *, int *, int *, MPI_Status *);
int MPI_Test_cancelled(MPI_Status *, int *);
int MPI_Sendrecv(void *, int, MPI_Datatype,int, int, void *, int, MPI_Datatype,
                 int, int, MPI_Comm, MPI_Status *);
int MPI_Sendrecv_replace(void*, int, MPI_Datatype, int, int, int, int, MPI_Comm,
                         MPI_Status *);
int MPI_Type_indexed(int, int *, int *, MPI_Datatype, MPI_Datatype *);
int MPI_Type_hindexed(int, int *, MPI_Aint *, MPI_Datatype, MPI_Datatype *);
int MPI_Type_extent(MPI_Datatype, MPI_Aint *);
int MPI_Type_size(MPI_Datatype, int *);
int MPI_Type_lb(MPI_Datatype, MPI_Aint *);
int MPI_Type_ub(MPI_Datatype, MPI_Aint *);
int MPI_Get(void *, int, MPI_Datatype, int, MPI_Aint, int, MPI_Datatype,
            MPI_Win);
int MPI_Put(void *, int, MPI_Datatype, int, MPI_Aint, int, MPI_Datatype,
            MPI_Win);
int MPI_Get_elements(MPI_Status *, MPI_Datatype, int *);
int MPI_Pack(void*, int, MPI_Datatype, void *, int, int *,  MPI_Comm);
int MPI_Unpack(void*, int, int *, void *, int, MPI_Datatype, MPI_Comm);
int MPI_Pack_size(int, MPI_Datatype, MPI_Comm, int *);
int MPI_Op_create(MPI_User_function *, int, MPI_Op *);
int MPI_Op_free( MPI_Op *);
int MPI_Group_size(MPI_Group, int *);
int MPI_Group_rank(MPI_Group, int *);
int MPI_Group_translate_ranks (MPI_Group, int, int *, MPI_Group, int *);
int MPI_Group_compare(MPI_Group, MPI_Group, int *);
int MPI_Group_union(MPI_Group, MPI_Group, MPI_Group *);
int MPI_Group_intersection(MPI_Group, MPI_Group, MPI_Group *);
int MPI_Group_difference(MPI_Group, MPI_Group, MPI_Group *);
int MPI_Group_range_excl(MPI_Group, int, int [][3], MPI_Group *);
int MPI_Comm_test_inter(MPI_Comm, int *);
int MPI_Comm_remote_group(MPI_Comm, MPI_Group *);
int MPI_Intercomm_merge(MPI_Comm, int, MPI_Comm *);
int MPI_Attr_delete(MPI_Comm, int);
int MPI_Dims_create(int, int, int *);
int MPI_Graph_create(MPI_Comm, int, int *, int *, int, MPI_Comm *);
int MPI_Graphdims_get(MPI_Comm, int *, int *);
int MPI_Graph_get(MPI_Comm, int, int, int *, int *);
int MPI_Cart_sub(MPI_Comm, int *, MPI_Comm *);
int MPI_Cart_map(MPI_Comm, int, int *, int *, int *);
int MPI_Graph_map(MPI_Comm, int, int *, int *, int *);
int MPI_Get_processor_name(char *, int *);
int MPI_Get_version(int *, int *);
int MPI_Errhandler_create(MPI_Handler_function *, MPI_Errhandler *);
int MPI_Errhandler_set(MPI_Comm, MPI_Errhandler);
int MPI_Errhandler_get(MPI_Comm, MPI_Errhandler *);
int MPI_Errhandler_free(MPI_Errhandler *);
int MPI_Pcontrol(const int, ...);
int MPI_Close_port(char *);
int MPI_Comm_accept(char *, MPI_Info, int, MPI_Comm, MPI_Comm *);
int MPI_Comm_connect(char *, MPI_Info, int, MPI_Comm, MPI_Comm *);
int MPI_Comm_disconnect(MPI_Comm *);
int MPI_Comm_get_parent(MPI_Comm *);
int MPI_Comm_join(int, MPI_Comm *);
int MPI_Comm_spawn(char *, char *[], int, MPI_Info, int, MPI_Comm, MPI_Comm *,
                   int array_of_errcodes[]);
int MPI_Comm_spawn_multiple(int, char *[], char **[], int [], MPI_Info [], int,
                            MPI_Comm, MPI_Comm *, int []);
int MPI_Lookup_name(char *, MPI_Info, char *);
int MPI_Open_port(MPI_Info, char *);
int MPI_Publish_name(char *, MPI_Info, char *);
int MPI_Unpublish_name(char *, MPI_Info, char *);
int MPI_Accumulate(void *, int, MPI_Datatype, int, MPI_Aint, int,MPI_Datatype,
                   MPI_Op, MPI_Win);
int MPI_Win_complete(MPI_Win);
int MPI_Win_create(void *, MPI_Aint, int, MPI_Info, MPI_Comm, MPI_Win *);
int MPI_Win_fence(int, MPI_Win);
int MPI_Win_free(MPI_Win *);
int MPI_Win_get_group(MPI_Win, MPI_Group *);
int MPI_Win_lock(int, int, int, MPI_Win);
int MPI_Win_post(MPI_Group, int, MPI_Win);
int MPI_Win_start(MPI_Group, int, MPI_Win);
int MPI_Win_test(MPI_Win, int *);
int MPI_Win_unlock(int, MPI_Win);
int MPI_Win_wait(MPI_Win);
int MPI_Alltoallw(void *, int [], int [], MPI_Datatype [], void *, int [],
                  int [], MPI_Datatype [], MPI_Comm);
int MPI_Exscan(void *, void *, int, MPI_Datatype, MPI_Op, MPI_Comm);
int MPI_Add_error_class(int *);
int MPI_Add_error_code(int, int *);
int MPI_Add_error_string(int, char *);
int MPI_Comm_call_errhandler(MPI_Comm, int);
int MPI_Comm_create_errhandler(MPI_Comm_errhandler_function *,
                               MPI_Errhandler *);
int MPI_Comm_create_keyval(MPI_Comm_copy_attr_function *,
                           MPI_Comm_delete_attr_function *, int *, void *);
int MPI_Comm_delete_attr(MPI_Comm, int);
int MPI_Comm_free_keyval(int *);
int MPI_Comm_get_attr(MPI_Comm, int, void *, int *);
int MPI_Comm_get_name(MPI_Comm, char *, int *);
int MPI_Comm_set_attr(MPI_Comm, int, void *);
int MPI_Comm_set_name(MPI_Comm, char *);
int MPI_File_call_errhandler(MPI_File, int);
int MPI_Grequest_complete(MPI_Request);
int MPI_Grequest_start(MPI_Grequest_query_function *,
                       MPI_Grequest_free_function *, MPI_Grequest_cancel_function *, void *,
                       MPI_Request *);
int MPI_Init_thread(int *, char ***, int, int *);
int MPI_Is_thread_main(int *);
int MPI_Query_thread(int *);
int MPI_Status_set_cancelled(MPI_Status *, int);
int MPI_Status_set_elements(MPI_Status *, MPI_Datatype, int);
int MPI_Type_create_keyval(MPI_Type_copy_attr_function *,
                           MPI_Type_delete_attr_function *, int *, void *);
int MPI_Type_delete_attr(MPI_Datatype, int);
int MPI_Type_free_keyval(int *);
int MPI_Type_get_attr(MPI_Datatype, int, void *, int *);
int MPI_Type_get_contents(MPI_Datatype, int, int, int, int [], MPI_Aint [],
                          MPI_Datatype []);
int MPI_Type_get_envelope(MPI_Datatype, int *, int *, int *, int *);
int MPI_Type_set_attr(MPI_Datatype, int, void *);
int MPI_Type_match_size( int, int, MPI_Datatype *);
int MPI_Win_call_errhandler(MPI_Win, int);
int MPI_Win_create_keyval(MPI_Win_copy_attr_function *,
                          MPI_Win_delete_attr_function *, int *, void *);
int MPI_Win_delete_attr(MPI_Win, int);
int MPI_Win_free_keyval(int *);
int MPI_Win_get_attr(MPI_Win, int, void *, int *);
int MPI_Win_get_name(MPI_Win, char *, int *);
int MPI_Win_set_attr(MPI_Win, int, void *);
int MPI_Win_set_name(MPI_Win, char *);
int MPI_File_c2f(MPI_File);
int MPI_Alloc_mem(MPI_Aint, MPI_Info info, void *baseptr);
int MPI_Comm_get_errhandler(MPI_Comm, MPI_Errhandler *);
int MPI_Comm_set_errhandler(MPI_Comm, MPI_Errhandler);
int MPI_File_create_errhandler(MPI_File_errhandler_function *,
                               MPI_Errhandler *);
int MPI_File_get_errhandler(MPI_File, MPI_Errhandler *);
int MPI_File_set_errhandler(MPI_File, MPI_Errhandler);
int MPI_Finalized(int *);
int MPI_Free_mem(void *);
int MPI_Get_address(void *, MPI_Aint *);
int MPI_Info_create(MPI_Info *);
int MPI_Info_delete(MPI_Info, char *);
int MPI_Info_dup(MPI_Info, MPI_Info *);
int MPI_Info_free(MPI_Info *info);
int MPI_Info_get(MPI_Info, char *, int, char *, int *);
int MPI_Info_get_nkeys(MPI_Info, int *);
int MPI_Info_get_nthkey(MPI_Info, int, char *);
int MPI_Info_get_valuelen(MPI_Info, char *, int *, int *);
int MPI_Info_set(MPI_Info, char *, char *);
int MPI_Pack_external(char *, void *, int, MPI_Datatype, void *, MPI_Aint,
                      MPI_Aint *);
int MPI_Pack_external_size(char *, int, MPI_Datatype, MPI_Aint *);
int MPI_Request_get_status(MPI_Request, int *, MPI_Status *);
int MPI_Type_create_darray(int, int, int, int [], int [], int [], int [], int,
                           MPI_Datatype, MPI_Datatype *);
int MPI_Type_create_hindexed(int, int [], MPI_Aint [], MPI_Datatype,
                             MPI_Datatype *);
int MPI_Type_create_hvector(int, int, MPI_Aint, MPI_Datatype, MPI_Datatype *);
int MPI_Type_create_indexed_block(int, int, int [], MPI_Datatype,
                                  MPI_Datatype *);
int MPI_Type_create_resized(MPI_Datatype, MPI_Aint, MPI_Aint, MPI_Datatype *);
int MPI_Type_create_struct(int, int [], MPI_Aint [],MPI_Datatype [],
                           MPI_Datatype *);
int MPI_Type_create_subarray(int, int [], int [], int [],int, MPI_Datatype,
                             MPI_Datatype *);
int MPI_Type_get_extent(MPI_Datatype, MPI_Aint *, MPI_Aint *);
int MPI_Type_get_true_extent(MPI_Datatype, MPI_Aint *, MPI_Aint *);
int MPI_Unpack_external(char *, void *, MPI_Aint, MPI_Aint *, void *,int,
                        MPI_Datatype);
int MPI_Win_create_errhandler(MPI_Win_errhandler_function *, MPI_Errhandler *);
int MPI_Win_get_errhandler(MPI_Win, MPI_Errhandler *);
int MPI_Win_set_errhandler(MPI_Win, MPI_Errhandler);
int MPI_Reduce_local(void *inbuf, void *inoutbuf, int count,
                     MPI_Datatype datatype, MPI_Op op);
int MPI_Op_commutative(MPI_Op op, int *commute);
int MPI_Reduce_scatter_block(void *sendbuf, void *recvbuf, int recvcount,
                             MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, int [],
                                   int [], int outdegree, int [], int [], MPI_Info info, int reorder,
                                   MPI_Comm *comm_dist_graph);
int MPI_Dist_graph_create(MPI_Comm comm_old, int n, int [], int [], int [],
                          int [], MPI_Info info, int reorder, MPI_Comm *comm_dist_graph);
int MPI_Dist_graph_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree,
                                   int *weighted);
int MPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int [], int [],
                             int maxoutdegree, int [], int []);

#ifdef __cplusplus
}
#endif

#endif

