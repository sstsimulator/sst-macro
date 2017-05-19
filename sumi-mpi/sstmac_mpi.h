/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_SSTMPI_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_SSTMPI_H_INCLUDED


#include <sumi-mpi/sstmac_mpi_macro.h>
#include <sumi-mpi/mpi_integers.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_types.h>
#include <stddef.h>


#define MPI_LOCK_SHARED 0
#define MPI_LOCK_EXCLUSIVE 1

#ifdef __cplusplus
extern "C" {
#endif

int sstmac_send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
             MPI_Comm comm);
int sstmac_recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
             MPI_Comm comm, MPI_Status *status);
int sstmac_get_count(const MPI_Status *status, MPI_Datatype datatype, int *count);
int sstmac_bsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm);
int sstmac_ssend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm);
int sstmac_rsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm);
int sstmac_buffer_attach(void *buffer, int size);
int sstmac_buffer_detach(void *buffer_addr, int *size);
int sstmac_isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm, MPI_Request *request);
int sstmac_ibsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request);
int sstmac_issend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request);
int sstmac_irsend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
               MPI_Comm comm, MPI_Request *request);
int sstmac_irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
              MPI_Comm comm, MPI_Request *request);
int sstmac_wait(MPI_Request *request, MPI_Status *status);
int sstmac_test(MPI_Request *request, int *flag, MPI_Status *status);
int sstmac_request_free(MPI_Request *request);
int sstmac_waitany(int count, MPI_Request array_of_requests[], int *indx, MPI_Status *status);
int sstmac_testany(int count, MPI_Request array_of_requests[], int *indx, int *flag,
                MPI_Status *status);
int sstmac_waitall(int count, MPI_Request array_of_requests[], MPI_Status array_of_statuses[]);
int sstmac_testall(int count, MPI_Request array_of_requests[], int *flag,
                MPI_Status array_of_statuses[]);
int sstmac_waitsome(int incount, MPI_Request array_of_requests[], int *outcount,
                 int array_of_indices[], MPI_Status array_of_statuses[]);
int sstmac_testsome(int incount, MPI_Request array_of_requests[], int *outcount,
                 int array_of_indices[], MPI_Status array_of_statuses[]);
int sstmac_iprobe(int source, int tag, MPI_Comm comm, int *flag, MPI_Status *status);
int sstmac_probe(int source, int tag, MPI_Comm comm, MPI_Status *status);
int sstmac_cancel(MPI_Request *request);
int sstmac_test_cancelled(const MPI_Status *status, int *flag);
int sstmac_send_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                  MPI_Comm comm, MPI_Request *request);
int sstmac_bsend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request);
int sstmac_ssend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request);
int sstmac_rsend_init(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
                   MPI_Comm comm, MPI_Request *request);
int sstmac_recv_init(void *buf, int count, MPI_Datatype datatype, int source, int tag,
                  MPI_Comm comm, MPI_Request *request);
int sstmac_start(MPI_Request *request);
int sstmac_startall(int count, MPI_Request array_of_requests[]);
int sstmac_sendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, int dest,
                 int sendtag, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                 int source, int recvtag, MPI_Comm comm, MPI_Status *status);
int sstmac_sendrecv_replace(void *buf, int count, MPI_Datatype datatype, int dest,
                         int sendtag, int source, int recvtag, MPI_Comm comm,
                         MPI_Status *status);
int sstmac_type_contiguous(int count, MPI_Datatype oldtype, MPI_Datatype *newtype);
int sstmac_type_vector(int count, int blocklength, int stride, MPI_Datatype oldtype,
                    MPI_Datatype *newtype);
int sstmac_type_hvector(int count, int blocklength, MPI_Aint stride, MPI_Datatype oldtype,
                     MPI_Datatype *newtype);
int sstmac_type_indexed(int count, const int *array_of_blocklengths,
                     const int *array_of_displacements, MPI_Datatype oldtype,
                     MPI_Datatype *newtype);
int sstmac_type_hindexed(int count, const int *array_of_blocklengths,
                      const MPI_Aint *array_of_displacements, MPI_Datatype oldtype,
                      MPI_Datatype *newtype);
int sstmac_type_struct(int count, const int *array_of_blocklengths,
                    const MPI_Aint *array_of_displacements,
                    const MPI_Datatype *array_of_types, MPI_Datatype *newtype);
int sstmac_address(const void *location, MPI_Aint *address);
int sstmac_type_extent(MPI_Datatype datatype, MPI_Aint *extent);
int sstmac_type_size(MPI_Datatype datatype, int *size);
int sstmac_type_lb(MPI_Datatype datatype, MPI_Aint *displacement);
int sstmac_type_ub(MPI_Datatype datatype, MPI_Aint *displacement);
int sstmac_type_commit(MPI_Datatype *datatype);
int sstmac_type_free(MPI_Datatype *datatype);
int sstmac_get_elements(const MPI_Status *status, MPI_Datatype datatype, int *count);
int sstmac_pack(const void *inbuf, int incount, MPI_Datatype datatype, void *outbuf,
             int outsize, int *position, MPI_Comm comm);
int sstmac_unpack(const void *inbuf, int insize, int *position, void *outbuf, int outcount,
               MPI_Datatype datatype, MPI_Comm comm);
int sstmac_pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm, int *size);
int sstmac_barrier(MPI_Comm comm);
int sstmac_bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm);
int sstmac_gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
               int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);
int sstmac_gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                const int *recvcounts, const int *displs, MPI_Datatype recvtype, int root,
                MPI_Comm comm);
int sstmac_scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm);
int sstmac_scatterv(const void *sendbuf, const int *sendcounts, const int *displs,
                 MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                 int root, MPI_Comm comm);
int sstmac_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                   const int *recvcounts, const int *displs, MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_alltoallv(const void *sendbuf, const int *sendcounts, const int *sdispls,
                  MPI_Datatype sendtype, void *recvbuf, const int *recvcounts,
                  const int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_alltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                  const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                  const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm);
int sstmac_exscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, MPI_Comm comm);
int sstmac_reduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
               MPI_Op op, int root, MPI_Comm comm);
int sstmac_op_create(MPI_User_function *user_fn, int commute, MPI_Op *op);
int sstmac_op_free(MPI_Op *op);
int sstmac_allreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                  MPI_Op op, MPI_Comm comm);
int sstmac_reduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
                       MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int sstmac_scan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
             MPI_Comm comm);
int sstmac_group_size(MPI_Group group, int *size);
int sstmac_group_rank(MPI_Group group, int *rank);
int sstmac_group_translate_ranks(MPI_Group group1, int n, const int ranks1[], MPI_Group group2,
                              int ranks2[]);
int sstmac_group_compare(MPI_Group group1, MPI_Group group2, int *result);
int sstmac_comm_group(MPI_Comm comm, MPI_Group *group);
int sstmac_group_union(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);
int sstmac_group_intersection(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);
int sstmac_group_difference(MPI_Group group1, MPI_Group group2, MPI_Group *newgroup);
int sstmac_group_incl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup);
int sstmac_group_excl(MPI_Group group, int n, const int ranks[], MPI_Group *newgroup);
int sstmac_group_range_incl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup);
int sstmac_group_range_excl(MPI_Group group, int n, int ranges[][3], MPI_Group *newgroup);
int sstmac_group_free(MPI_Group *group);
int sstmac_comm_size(MPI_Comm comm, int *size);
int sstmac_comm_rank(MPI_Comm comm, int *rank);
int sstmac_comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result);
int sstmac_comm_dup(MPI_Comm comm, MPI_Comm *newcomm);
int sstmac_comm_dup_with_info(MPI_Comm comm, MPI_Info info, MPI_Comm *newcomm);
int sstmac_comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm);
int sstmac_comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm);
int sstmac_comm_free(MPI_Comm *comm);
int sstmac_comm_test_inter(MPI_Comm comm, int *flag);
int sstmac_comm_remote_size(MPI_Comm comm, int *size);
int sstmac_comm_remote_group(MPI_Comm comm, MPI_Group *group);
int sstmac_intercomm_create(MPI_Comm local_comm, int local_leader, MPI_Comm peer_comm,
                         int remote_leader, int tag, MPI_Comm *newintercomm);
int sstmac_intercomm_merge(MPI_Comm intercomm, int high, MPI_Comm *newintracomm);
int sstmac_keyval_create(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn,
                      int *keyval, void *extra_state);
int sstmac_keyval_free(int *keyval);
int sstmac_attr_put(MPI_Comm comm, int keyval, void *attribute_val);
int sstmac_attr_get(MPI_Comm comm, int keyval, void *attribute_val, int *flag);
int sstmac_attr_delete(MPI_Comm comm, int keyval);
int sstmac_topo_test(MPI_Comm comm, int *status);
int sstmac_cart_create(MPI_Comm comm_old, int ndims, const int dims[], const int periods[],
                    int reorder, MPI_Comm *comm_cart);
int sstmac_dims_create(int nnodes, int ndims, int dims[]);
int sstmac_graph_create(MPI_Comm comm_old, int nnodes, const int indx[], const int edges[],
                     int reorder, MPI_Comm *comm_graph);
int sstmac_graphdims_get(MPI_Comm comm, int *nnodes, int *nedges);
int sstmac_graph_get(MPI_Comm comm, int maxindex, int maxedges, int indx[], int edges[]);
int sstmac_cartdim_get(MPI_Comm comm, int *ndims);
int sstmac_cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[], int coords[]);
int sstmac_cart_rank(MPI_Comm comm, const int coords[], int *rank);
int sstmac_cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]);
int sstmac_graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors);
int sstmac_graph_neighbors(MPI_Comm comm, int rank, int maxneighbors, int neighbors[]);
int sstmac_cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source, int *rank_dest);
int sstmac_cart_sub(MPI_Comm comm, const int remain_dims[], MPI_Comm *newcomm);
int sstmac_cart_map(MPI_Comm comm, int ndims, const int dims[], const int periods[], int *newrank);
int sstmac_graph_map(MPI_Comm comm, int nnodes, const int indx[], const int edges[], int *newrank);
int sstmac_get_processor_name(char *name, int *resultlen);
int sstmac_get_version(int *version, int *subversion);
int sstmac_get_library_version(char *version, int *resultlen);
int sstmac_errhandler_create(MPI_Handler_function *function, MPI_Errhandler *errhandler);
int sstmac_errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler);
int sstmac_errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler);
int sstmac_errhandler_free(MPI_Errhandler *errhandler);
int sstmac_error_string(int errorcode, char *string, int *resultlen);
int sstmac_error_class(int errorcode, int *errorclass);
double sstmac_wtime(void);
double sstmac_wtick(void);
int sstmac_init(int *argc, char ***argv);
int sstmac_finalize(void);
int sstmac_initialized(int *flag);
int sstmac_abort(MPI_Comm comm, int errorcode);

/* Note that we may need to define a @PCONTROL_LIST@ depending on whether
   stdargs are supported */
int sstmac_pcontrol(const int level, ...);
int sstmac_dUP_FN(MPI_Comm oldcomm, int keyval, void *extra_state, void *attribute_val_in,
               void *attribute_val_out, int *flag);

/* Process Creation and Management */
int sstmac_close_port(const char *port_name);
int sstmac_comm_accept(const char *port_name, MPI_Info info, int root, MPI_Comm comm,
                    MPI_Comm *newcomm);
int sstmac_comm_connect(const char *port_name, MPI_Info info, int root, MPI_Comm comm,
                     MPI_Comm *newcomm);
int sstmac_comm_disconnect(MPI_Comm *comm);
int sstmac_comm_get_parent(MPI_Comm *parent);
int sstmac_comm_join(int fd, MPI_Comm *intercomm);
int sstmac_comm_spawn(const char *command, char *argv[], int maxprocs, MPI_Info info, int root,
                   MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]);
int sstmac_comm_spawn_multiple(int count, char *array_of_commands[], char **array_of_argv[],
                            const int array_of_maxprocs[], const MPI_Info array_of_info[],
                            int root, MPI_Comm comm, MPI_Comm *intercomm, int array_of_errcodes[]);
int sstmac_lookup_name(const char *service_name, MPI_Info info, char *port_name);
int sstmac_open_port(MPI_Info info, char *port_name);
int sstmac_publish_name(const char *service_name, MPI_Info info, const char *port_name);
int sstmac_unpublish_name(const char *service_name, MPI_Info info, const char *port_name);
int sstmac_comm_set_info(MPI_Comm comm, MPI_Info info);
int sstmac_comm_get_info(MPI_Comm comm, MPI_Info *info);

/* One-Sided Communications */
int sstmac_accumulate(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
                   int target_rank, MPI_Aint target_disp, int target_count,
                   MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);
int sstmac_get(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
            int target_rank, MPI_Aint target_disp, int target_count,
            MPI_Datatype target_datatype, MPI_Win win);
int sstmac_put(const void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
            int target_rank, MPI_Aint target_disp, int target_count,
            MPI_Datatype target_datatype, MPI_Win win);
int sstmac_win_complete(MPI_Win win);
int sstmac_win_create(void *base, MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm,
                   MPI_Win *win);
int sstmac_win_fence(int assert, MPI_Win win);
int sstmac_win_free(MPI_Win *win);
int sstmac_win_get_group(MPI_Win win, MPI_Group *group);
int sstmac_win_lock(int lock_type, int rank, int assert, MPI_Win win);
int sstmac_win_post(MPI_Group group, int assert, MPI_Win win);
int sstmac_win_start(MPI_Group group, int assert, MPI_Win win);
int sstmac_win_test(MPI_Win win, int *flag);
int sstmac_win_unlock(int rank, MPI_Win win);
int sstmac_win_wait(MPI_Win win);

/* MPI-3 One-Sided Communication Routines */
int sstmac_win_allocate(MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm, void *baseptr,
                     MPI_Win *win);
int sstmac_win_allocate_shared(MPI_Aint size, int disp_unit, MPI_Info info, MPI_Comm comm,
                            void *baseptr, MPI_Win *win);
int sstmac_win_shared_query(MPI_Win win, int rank, MPI_Aint *size, int *disp_unit, void *baseptr);
int sstmac_win_create_dynamic(MPI_Info info, MPI_Comm comm, MPI_Win *win);
int sstmac_win_attach(MPI_Win win, void *base, MPI_Aint size);
int sstmac_win_detach(MPI_Win win, const void *base);
int sstmac_win_get_info(MPI_Win win, MPI_Info *info_used);
int sstmac_win_set_info(MPI_Win win, MPI_Info info);
int sstmac_get_accumulate(const void *origin_addr, int origin_count,
                        MPI_Datatype origin_datatype, void *result_addr, int result_count,
                        MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp,
                        int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);
int sstmac_fetch_and_op(const void *origin_addr, void *result_addr,
                      MPI_Datatype datatype, int target_rank, MPI_Aint target_disp,
                      MPI_Op op, MPI_Win win);
int sstmac_compare_and_swap(const void *origin_addr, const void *compare_addr,
                          void *result_addr, MPI_Datatype datatype, int target_rank,
                          MPI_Aint target_disp, MPI_Win win);
int sstmac_rput(const void *origin_addr, int origin_count,
              MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
              int target_count, MPI_Datatype target_datatype, MPI_Win win,
              MPI_Request *request);
int sstmac_rget(void *origin_addr, int origin_count,
              MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
              int target_count, MPI_Datatype target_datatype, MPI_Win win,
              MPI_Request *request);
int sstmac_raccumulate(const void *origin_addr, int origin_count,
                     MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
                     int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                     MPI_Request *request);
int sstmac_rget_accumulate(const void *origin_addr, int origin_count,
                         MPI_Datatype origin_datatype, void *result_addr, int result_count,
                         MPI_Datatype result_datatype, int target_rank, MPI_Aint target_disp,
                         int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                         MPI_Request *request);
int sstmac_win_lock_all(int assert, MPI_Win win);
int sstmac_win_unlock_all(MPI_Win win);
int sstmac_win_flush(int rank, MPI_Win win);
int sstmac_win_flush_all(MPI_Win win);
int sstmac_win_flush_local(int rank, MPI_Win win);
int sstmac_win_flush_local_all(MPI_Win win);
int sstmac_win_sync(MPI_Win win);
 
/* External Interfaces */
int sstmac_add_error_class(int *errorclass);
int sstmac_add_error_code(int errorclass, int *errorcode);
int sstmac_add_error_string(int errorcode, const char *string);
int sstmac_comm_call_errhandler(MPI_Comm comm, int errorcode);
int sstmac_comm_create_keyval(MPI_Comm_copy_attr_function *comm_copy_attr_fn,
                           MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval,
                           void *extra_state);
int sstmac_comm_delete_attr(MPI_Comm comm, int comm_keyval);
int sstmac_comm_free_keyval(int *comm_keyval);
int sstmac_comm_get_attr(MPI_Comm comm, int comm_keyval, void *attribute_val, int *flag);
int sstmac_comm_get_name(MPI_Comm comm, char *comm_name, int *resultlen);
int sstmac_comm_set_attr(MPI_Comm comm, int comm_keyval, void *attribute_val);
int sstmac_comm_set_name(MPI_Comm comm, const char *comm_name);
int sstmac_file_call_errhandler(MPI_File fh, int errorcode);
int sstmac_grequest_complete(MPI_Request request);
int sstmac_grequest_start(MPI_Grequest_query_function *query_fn, MPI_Grequest_free_function *free_fn,
                       MPI_Grequest_cancel_function *cancel_fn, void *extra_state,
                       MPI_Request *request);
int sstmac_init_thread(int *argc, char ***argv, int required, int *provided);
int sstmac_is_thread_main(int *flag);
int sstmac_query_thread(int *provided);
int sstmac_status_set_cancelled(MPI_Status *status, int flag);
int sstmac_status_set_elements(MPI_Status *status, MPI_Datatype datatype, int count);
int sstmac_type_create_keyval(MPI_Type_copy_attr_function *type_copy_attr_fn,
                           MPI_Type_delete_attr_function *type_delete_attr_fn,
                           int *type_keyval, void *extra_state);
int sstmac_type_delete_attr(MPI_Datatype datatype, int type_keyval);
int sstmac_type_dup(MPI_Datatype oldtype, MPI_Datatype *newtype);
int sstmac_type_free_keyval(int *type_keyval);
int sstmac_type_get_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val, int *flag);
int sstmac_type_get_contents(MPI_Datatype datatype, int max_integers, int max_addresses,
                          int max_datatypes, int array_of_integers[],
                          MPI_Aint array_of_addresses[], MPI_Datatype array_of_datatypes[]);
int sstmac_type_get_envelope(MPI_Datatype datatype, int *num_integers, int *num_addresses,
                          int *num_datatypes, int *combiner);
int sstmac_type_get_name(MPI_Datatype datatype, char *type_name, int *resultlen);
int sstmac_type_set_attr(MPI_Datatype datatype, int type_keyval, void *attribute_val);
int sstmac_type_set_name(MPI_Datatype datatype, const char *type_name);
int sstmac_type_match_size(int typeclass, int size, MPI_Datatype *datatype);
int sstmac_win_call_errhandler(MPI_Win win, int errorcode);
int sstmac_win_create_keyval(MPI_Win_copy_attr_function *win_copy_attr_fn,
                          MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval,
                          void *extra_state);
int sstmac_win_delete_attr(MPI_Win win, int win_keyval);
int sstmac_win_free_keyval(int *win_keyval);
int sstmac_win_get_attr(MPI_Win win, int win_keyval, void *attribute_val, int *flag);
int sstmac_win_get_name(MPI_Win win, char *win_name, int *resultlen);
int sstmac_win_set_attr(MPI_Win win, int win_keyval, void *attribute_val);
int sstmac_win_set_name(MPI_Win win, const char *win_name);

int sstmac_alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr);
int sstmac_comm_create_errhandler(MPI_Comm_errhandler_function *comm_errhandler_fn,
                               MPI_Errhandler *errhandler);
int sstmac_comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler);
int sstmac_comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler);
int sstmac_file_create_errhandler(MPI_File_errhandler_function *file_errhandler_fn,
                               MPI_Errhandler *errhandler);
int sstmac_file_get_errhandler(MPI_File file, MPI_Errhandler *errhandler);
int sstmac_file_set_errhandler(MPI_File file, MPI_Errhandler errhandler);
int sstmac_finalized(int *flag);
int sstmac_free_mem(void *base);
int sstmac_get_address(const void *location, MPI_Aint *address);
int sstmac_info_create(MPI_Info *info);
int sstmac_info_delete(MPI_Info info, const char *key);
int sstmac_info_dup(MPI_Info info, MPI_Info *newinfo);
int sstmac_info_free(MPI_Info *info);
int sstmac_info_get(MPI_Info info, const char *key, int valuelen, char *value, int *flag);
int sstmac_info_get_nkeys(MPI_Info info, int *nkeys);
int sstmac_info_get_nthkey(MPI_Info info, int n, char *key);
int sstmac_info_get_valuelen(MPI_Info info, const char *key, int *valuelen, int *flag);
int sstmac_info_set(MPI_Info info, const char *key, const char *value);
int sstmac_pack_external(const char datarep[], const void *inbuf, int incount,
                      MPI_Datatype datatype, void *outbuf, MPI_Aint outsize, MPI_Aint *position);
int sstmac_pack_external_size(const char datarep[], int incount, MPI_Datatype datatype,
                           MPI_Aint *size);
int sstmac_request_get_status(MPI_Request request, int *flag, MPI_Status *status);
int sstmac_status_c2f(const MPI_Status *c_status, MPI_Fint *f_status);
int sstmac_status_f2c(const MPI_Fint *f_status, MPI_Status *c_status);
int sstmac_type_create_darray(int size, int rank, int ndims, const int array_of_gsizes[],
                           const int array_of_distribs[], const int array_of_dargs[],
                           const int array_of_psizes[], int order, MPI_Datatype oldtype,
                           MPI_Datatype *newtype);
int sstmac_type_create_hindexed(int count, const int array_of_blocklengths[],
                             const MPI_Aint array_of_displacements[], MPI_Datatype oldtype,
                             MPI_Datatype *newtype);
int sstmac_type_create_hvector(int count, int blocklength, MPI_Aint stride, MPI_Datatype oldtype,
                            MPI_Datatype *newtype);
int sstmac_type_create_indexed_block(int count, int blocklength, const int array_of_displacements[],
                                  MPI_Datatype oldtype, MPI_Datatype *newtype);
int sstmac_type_create_hindexed_block(int count, int blocklength,
                                   const MPI_Aint array_of_displacements[],
                                   MPI_Datatype oldtype, MPI_Datatype *newtype);
int sstmac_type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, MPI_Aint extent,
                            MPI_Datatype *newtype);
int sstmac_type_create_struct(int count, const int array_of_blocklengths[],
                           const MPI_Aint array_of_displacements[],
                           const MPI_Datatype array_of_types[], MPI_Datatype *newtype);
int sstmac_type_create_subarray(int ndims, const int array_of_sizes[],
                             const int array_of_subsizes[], const int array_of_starts[],
                             int order, MPI_Datatype oldtype, MPI_Datatype *newtype);
int sstmac_type_get_extent(MPI_Datatype datatype, MPI_Aint *lb, MPI_Aint *extent);
int sstmac_type_get_true_extent(MPI_Datatype datatype, MPI_Aint *true_lb, MPI_Aint *true_extent);
int sstmac_unpack_external(const char datarep[], const void *inbuf, MPI_Aint insize,
                        MPI_Aint *position, void *outbuf, int outcount, MPI_Datatype datatype);
int sstmac_win_create_errhandler(MPI_Win_errhandler_function *win_errhandler_fn,
                              MPI_Errhandler *errhandler);
int sstmac_win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler);
int sstmac_win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler);

/* Fortran 90-related functions.  These routines are available only if
   Fortran 90 support is enabled 
*/
int sstmac_type_create_f90_integer(int range, MPI_Datatype *newtype);
int sstmac_type_create_f90_real(int precision, int range, MPI_Datatype *newtype);
int sstmac_type_create_f90_complex(int precision, int range, MPI_Datatype *newtype);

int sstmac_reduce_local(const void *inbuf, void *inoutbuf, int count, MPI_Datatype datatype,
                     MPI_Op op);
int sstmac_op_commutative(MPI_Op op, int *commute);
int sstmac_reduce_scatter_block(const void *sendbuf, void *recvbuf, int recvcount,
                             MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);
int sstmac_dist_graph_create_adjacent(MPI_Comm comm_old, int indegree, const int sources[],
                                   const int sourceweights[], int outdegree,
                                   const int destinations[], const int destweights[],
                                   MPI_Info info, int reorder, MPI_Comm *comm_dist_graph);
int sstmac_dist_graph_create(MPI_Comm comm_old, int n, const int sources[], const int degrees[],
                          const int destinations[], const int weights[], MPI_Info info,
                          int reorder, MPI_Comm *comm_dist_graph);
int sstmac_dist_graph_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted);
int sstmac_dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[],
                             int maxoutdegree, int destinations[], int destweights[]);

/* Matched probe functionality */
int sstmac_improbe(int source, int tag, MPI_Comm comm, int *flag, MPI_Message *message,
                MPI_Status *status);
int sstmac_imrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message,
               MPI_Request *request);
int sstmac_mprobe(int source, int tag, MPI_Comm comm, MPI_Message *message, MPI_Status *status);
int sstmac_mrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message,
              MPI_Status *status);

/* Nonblocking collectives */
int sstmac_comm_idup(MPI_Comm comm, MPI_Comm *newcomm, MPI_Request *request);
int sstmac_ibarrier(MPI_Comm comm, MPI_Request *request);
int sstmac_ibcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm,
               MPI_Request *request);
int sstmac_igather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                MPI_Request *request);
int sstmac_igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 const int recvcounts[], const int displs[], MPI_Datatype recvtype, int root,
                 MPI_Comm comm, MPI_Request *request);
int sstmac_iscatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                 int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm,
                 MPI_Request *request);
int sstmac_iscatterv(const void *sendbuf, const int sendcounts[], const int displs[],
                  MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
                  int root, MPI_Comm comm, MPI_Request *request);
int sstmac_iallgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                   int recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int sstmac_iallgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                    const int recvcounts[], const int displs[], MPI_Datatype recvtype,
                    MPI_Comm comm, MPI_Request *request);
int sstmac_ialltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf,
                  int recvcount, MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int sstmac_ialltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                   MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                   const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
                   MPI_Request *request);
int sstmac_ialltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                   const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                   const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm,
                   MPI_Request *request);
int sstmac_ireduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                MPI_Op op, int root, MPI_Comm comm, MPI_Request *request);
int sstmac_iallreduce(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                   MPI_Op op, MPI_Comm comm, MPI_Request *request);
int sstmac_ireduce_scatter(const void *sendbuf, void *recvbuf, const int recvcounts[],
                        MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, MPI_Request *request);
int sstmac_ireduce_scatter_block(const void *sendbuf, void *recvbuf, int recvcount,
                              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                              MPI_Request *request);
int sstmac_iscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype, MPI_Op op,
              MPI_Comm comm, MPI_Request *request);
int sstmac_iexscan(const void *sendbuf, void *recvbuf, int count, MPI_Datatype datatype,
                MPI_Op op, MPI_Comm comm, MPI_Request *request);

/* Neighborhood collectives */
int sstmac_ineighbor_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, int recvcount, MPI_Datatype recvtype,
                            MPI_Comm comm, MPI_Request *request);
int sstmac_ineighbor_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                             void *recvbuf, const int recvcounts[], const int displs[],
                             MPI_Datatype recvtype, MPI_Comm comm, MPI_Request *request);
int sstmac_ineighbor_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                           void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm,
                           MPI_Request *request);
int sstmac_ineighbor_alltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                            MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                            const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm,
                            MPI_Request *request);
int sstmac_ineighbor_alltoallw(const void *sendbuf, const int sendcounts[],
                            const MPI_Aint sdispls[], const MPI_Datatype sendtypes[],
                            void *recvbuf, const int recvcounts[], const MPI_Aint rdispls[],
                            const MPI_Datatype recvtypes[], MPI_Comm comm, MPI_Request *request);
int sstmac_neighbor_allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                           void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_neighbor_allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, const int recvcounts[], const int displs[],
                            MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_neighbor_alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                          void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_neighbor_alltoallv(const void *sendbuf, const int sendcounts[], const int sdispls[],
                           MPI_Datatype sendtype, void *recvbuf, const int recvcounts[],
                           const int rdispls[], MPI_Datatype recvtype, MPI_Comm comm);
int sstmac_neighbor_alltoallw(const void *sendbuf, const int sendcounts[], const MPI_Aint sdispls[],
                           const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                           const MPI_Aint rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm);
#ifdef __cplusplus
}
#endif


#endif