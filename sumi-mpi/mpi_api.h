/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPI_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPI_H_INCLUDED

#include <sstmac/software/libraries/library.h>
#include <sstmac/software/api/api.h>

#include <sumi/message_fwd.h>

#include <sumi-mpi/mpi_types.h>
#include <sumi-mpi/mpi_integers.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_types/mpi_type_fwd.h>

#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_call.h>
#include <sumi-mpi/mpi_comm/mpi_comm_factory.h>
#include <sumi-mpi/mpi_debug.h>
#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>

#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/software/process/ftq.h>

#include <sprockit/sim_parameters_fwd.h>
#include <unordered_map>
#include <sprockit/factories/factory.h>

#include <sstmac/libraries/sumi/sumi_transport.h>


namespace sumi {

using sstmac::sw::software_id;
using sstmac::sw::operating_system;

class mpi_api :
  public sstmac::sumi_transport
{
  RegisterAPI("mpi", mpi_api)

 public:
  mpi_api(sprockit::sim_parameters* params,
          sstmac::sw::software_id sid,
          sstmac::sw::operating_system* os);

  static void delete_statics();

 public:
  virtual ~mpi_api();

  mpi_queue* queue() {
    return queue_;
  }

  /**
   * @brief crossed_comm_world_barrier
   *        Useful for statistics based on globally synchronous timings.
   * @return Whether a global collective has been crossed
   *
   */
  bool crossed_comm_world_barrier() const {
    return crossed_comm_world_barrier_;
  }

  mpi_comm* comm_world() const {
    return worldcomm_;
  }

  mpi_comm* comm_self() const {
    return selfcomm_;
  }

  int comm_get_attr(MPI_Comm, int comm_keyval, void* attribute_val, int* flag);

  int comm_rank(MPI_Comm comm, int* rank);

  int comm_size(MPI_Comm comm, int* size);

  int type_size(MPI_Datatype type, int* size);

  int initialized(int* flag){
    *flag = (status_ == is_initialized);
    return MPI_SUCCESS;
  }

  int finalized(int* flag){
    *flag = (status_ == is_finalized);
    return MPI_SUCCESS;
  }

  int buffer_attach(void* buffer, int size){
    return MPI_SUCCESS;
  }

  int buffer_detach(void* buffer, int* size){
    return MPI_SUCCESS;
  }

  int init_thread(int* argc, char*** argv, int required, int* provided){
    init(argc, argv);
    *provided = required;
    return MPI_SUCCESS;
  }

#pragma GCC diagnostic ignored "-Woverloaded-virtual"
  int init(int* argc, char*** argv);

  int finalize();

  double wtime();

  void set_generate_ids(bool flag){
    generate_ids_ = flag;
  }

  bool generate_ids() const {
    return generate_ids_;
  }

  int abort(MPI_Comm comm, int errcode);

  int errhandler_set(MPI_Comm comm, MPI_Errhandler handler){
    return MPI_SUCCESS;
  }

  int errhandler_create(MPI_Handler_function *function, MPI_Errhandler *errhandler){
    return MPI_SUCCESS;
  }

  int error_class(int errorcode, int* errorclass){
    *errorclass = 0;
    return MPI_SUCCESS;
  }

  int error_string(int errorcode, char* str, int* resultlen);

  int comm_split(MPI_Comm incomm, int color, int key, MPI_Comm* outcomm);

  int comm_dup(MPI_Comm input, MPI_Comm* output);

  int comm_create(MPI_Comm input, MPI_Group group, MPI_Comm* output);

  int comm_group(MPI_Comm comm, MPI_Group* grp);

  int comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm * newcomm);

  void comm_create_with_id(MPI_Comm input, MPI_Group group, MPI_Comm new_comm);

  /**
   * @param group
   * @param num_members
   * @param members
   * @return Whether the current rank is in the group
   */
  bool group_create_with_id(MPI_Group group, int num_members, const uint32_t* members);

  int cart_create(MPI_Comm comm_old, int ndims, const int dims[],
              const int periods[], int reorder, MPI_Comm *comm_cart);

  int cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[], int coords[]);

  int cartdim_get(MPI_Comm comm, int *ndims);

  int cart_rank(MPI_Comm comm, const int coords[], int *rank);

  int cart_shift(MPI_Comm comm, int direction, int disp,
                 int *rank_source, int *rank_dest);

  int cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]);

  int comm_free(MPI_Comm* input);

  int comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler){
    return MPI_SUCCESS;
  }

  int group_incl(MPI_Group oldgrp,
             int num_ranks,
             const int* ranks,
             MPI_Group* newgrp);

  int group_range_incl(MPI_Group oldgrp, int n, int ranges[][3], MPI_Group* newgrp);

  int group_free(MPI_Group* grp);

  int group_translate_ranks(MPI_Group grp1, int n, const int* ranks1, MPI_Group grp2, int* ranks2);

  int sendrecv(const void* sendbuf, int sendcount,
        MPI_Datatype sendtype, int dest, int sendtag,
        void* recvbuf, int recvcount,
        MPI_Datatype recvtype, int source, int recvtag,
        MPI_Comm comm, MPI_Status* status);

  int send(const void *buf, int count,
           MPI_Datatype datatype, int dest, int tag,
           MPI_Comm comm);

  int send_init(const void *buf, int count, MPI_Datatype datatype, int dest,
                int tag, MPI_Comm comm, MPI_Request *request);

  int isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
            MPI_Comm comm, MPI_Request *request);

  int recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
           MPI_Comm comm, MPI_Status *status);

  int irecv(void *buf, int count, MPI_Datatype datatype, int source,
            int tag, MPI_Comm comm, MPI_Request *request);

  int recv_init(void *buf, int count, MPI_Datatype datatype,
      int source, int tag, MPI_Comm comm, MPI_Request *request);

  int request_free(MPI_Request* req);

  int start(MPI_Request* req);

  int startall(int count, MPI_Request* req);

  int wait(MPI_Request *request, MPI_Status *status);

  int waitall(int count, MPI_Request requests[], MPI_Status statuses[]);

  int waitany(int count, MPI_Request requests[], int *indx, MPI_Status *status);

  int waitsome(int incount, MPI_Request requests[],
           int *outcount, int indices[], MPI_Status statuses[]);

  int test(MPI_Request *request, int *flag, MPI_Status *status);

  int testall(int count, MPI_Request requests[], int *flag, MPI_Status statuses[]);

  int testany(int count, MPI_Request requests[], int *indx, int *flag, MPI_Status *status);

  int testsome(int incount, MPI_Request requests[], int *outcount,
               int indices[], MPI_Status statuses[]);

  int probe(int source, int tag, MPI_Comm comm, MPI_Status *status);

  int iprobe(int source, int tag, MPI_Comm comm, int* flag, MPI_Status *status);

  int barrier(MPI_Comm comm);

  int bcast(int count, MPI_Datatype datatype, int root, MPI_Comm comm);

  int bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm);

  int scatter(int sendcount, MPI_Datatype sendtype,
          int recvcount, MPI_Datatype recvtype, int root,
          MPI_Comm comm);

  int scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
           void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
           MPI_Comm comm);

  int scatterv(const int *sendcounts,
           MPI_Datatype sendtype, int recvcount,
           MPI_Datatype recvtype,
           int root, MPI_Comm comm);

  int scatterv(const void *sendbuf, const int *sendcounts, const int *displs,
           MPI_Datatype sendtype, void *recvbuf, int recvcount,
           MPI_Datatype recvtype,
           int root, MPI_Comm comm);

  int gather(int sendcount, MPI_Datatype sendtype,
         int recvcount, MPI_Datatype recvtype,
         int root, MPI_Comm comm);

  int gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
         void *recvbuf, int recvcount, MPI_Datatype recvtype,
         int root, MPI_Comm comm);

  int gatherv(int sendcount, MPI_Datatype sendtype,
         const int *recvcounts,
         MPI_Datatype recvtype, int root, MPI_Comm comm);

  int gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
          void *recvbuf, const int *recvcounts, const int *displs,
          MPI_Datatype recvtype, int root, MPI_Comm comm);

  int allgather(int count, MPI_Datatype type, MPI_Comm comm);

  int allgather(int sendcount, MPI_Datatype sendtype,
            int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
            void *recvbuf, int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int allgatherv(int sendcount, MPI_Datatype sendtype,
             const int *recvcounts,
             MPI_Datatype recvtype, MPI_Comm comm);

  int allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
             void *recvbuf, const int *recvcounts, const int *displs,
             MPI_Datatype recvtype, MPI_Comm comm);

  int alltoall(int sendcount, MPI_Datatype sendtype,
           int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
            void *recvbuf, int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int alltoallv(const int *sendcounts,
            MPI_Datatype sendtype,
            const int *recvcounts,
            MPI_Datatype recvtype,
            MPI_Comm comm);

  int alltoallv(const void *sendbuf, const int *sendcounts,
            const int *sdispls, MPI_Datatype sendtype, void *recvbuf,
            const int *recvcounts, const int *rdispls, MPI_Datatype recvtype,
            MPI_Comm comm);

  int reduce(int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm);

  int reduce(const void* src, void* dst,
         int count, MPI_Datatype type, MPI_Op op, int root,
         MPI_Comm comm);

  int allreduce(int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm);

  int allreduce(const void* src, void* dst,
            int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm);

  int scan(int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm);

  int scan(const void* src, void* dst,
            int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm);

  int reduce_scatter(int* recvcnts, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm);

  int reduce_scatter(const void* src, void* dst,
                 const int* recvcnts, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm);

  int reduce_scatter_block(int recvcnt, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm);

  int reduce_scatter_block(const void* src, void* dst,
                 int recvcnt, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm);

  int ibarrier(MPI_Comm comm, MPI_Request* req);

  int ibcast(int count, MPI_Datatype datatype, int root,
        MPI_Comm comm, MPI_Request* req);

  int ibcast(void *buffer, int count, MPI_Datatype datatype, int root,
        MPI_Comm comm, MPI_Request* req);

  int iscatter(int sendcount, MPI_Datatype sendtype,
          int recvcount, MPI_Datatype recvtype, int root,
          MPI_Comm comm, MPI_Request* req);

  int iscatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
           void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
           MPI_Comm comm, MPI_Request* req);

  int iscatterv(const int *sendcounts,
           MPI_Datatype sendtype, int recvcount,
           MPI_Datatype recvtype,
           int root, MPI_Comm comm, MPI_Request* req);

  int iscatterv(const void *sendbuf, const int *sendcounts, const int *displs,
           MPI_Datatype sendtype, void *recvbuf, int recvcount,
           MPI_Datatype recvtype,
           int root, MPI_Comm comm, MPI_Request* req);

  int igather(int sendcount, MPI_Datatype sendtype,
         int recvcount, MPI_Datatype recvtype,
         int root, MPI_Comm comm, MPI_Request* req);

  int igather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
         void *recvbuf, int recvcount, MPI_Datatype recvtype,
         int root, MPI_Comm comm, MPI_Request* req);

  int igatherv(int sendcount, MPI_Datatype sendtype,
         const int *recvcounts,
         MPI_Datatype recvtype, int root,
         MPI_Comm comm, MPI_Request* req);

  int igatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
          void *recvbuf, const int *recvcounts, const int *displs,
          MPI_Datatype recvtype, int root,
          MPI_Comm comm, MPI_Request* req);

  int iallgather(int sendcount, MPI_Datatype sendtype,
            int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm, MPI_Request* req);

  int iallgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
            void *recvbuf, int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm, MPI_Request* req);

  int iallgatherv(int sendcount, MPI_Datatype sendtype,
             const int *recvcounts,
             MPI_Datatype recvtype, MPI_Comm comm, MPI_Request* req);

  int iallgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
             void *recvbuf, const int *recvcounts, const int *displs,
             MPI_Datatype recvtype, MPI_Comm comm, MPI_Request* req);

  int ialltoall(int sendcount, MPI_Datatype sendtype,
            int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm, MPI_Request* req);

  int ialltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
            void *recvbuf, int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm, MPI_Request* req);

  int ialltoallw(const void *sendbuf, const int sendcounts[], const int sdispls[],
                 const MPI_Datatype sendtypes[], void *recvbuf, const int recvcounts[],
                 const int rdispls[], const MPI_Datatype recvtypes[], MPI_Comm comm,
                 MPI_Request *request);

  int ialltoallv(const int *sendcounts, MPI_Datatype sendtype,
                const int *recvcounts, MPI_Datatype recvtype,
                MPI_Comm comm, MPI_Request* req);

  int ialltoallv(const void *sendbuf, const int *sendcounts,
            const int *sdispls, MPI_Datatype sendtype, void *recvbuf,
            const int *recvcounts, const int *rdispls, MPI_Datatype recvtype,
            MPI_Comm comm, MPI_Request* req);

  int ireduce(int count, MPI_Datatype type, MPI_Op op, int root,
            MPI_Comm comm, MPI_Request* req);

  int ireduce(const void* src, void* dst,
         int count, MPI_Datatype type, MPI_Op op, int root,
         MPI_Comm comm, MPI_Request* req);

  int iallreduce(int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm, MPI_Request* req);

  int iallreduce(const void* src, void* dst,
            int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm, MPI_Request* req);

  int iscan(int count, MPI_Datatype type, MPI_Op op,
        MPI_Comm comm, MPI_Request* req);

  int iscan(const void* src, void* dst,
        int count, MPI_Datatype type, MPI_Op op,
        MPI_Comm comm, MPI_Request* req);

  int ireduce_scatter(int* recvcnts, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm, MPI_Request* req);

  int ireduce_scatter(const void* src, void* dst,
                 const int* recvcnts, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm, MPI_Request* req);

  int ireduce_scatter_block(int recvcnt, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm, MPI_Request* req);

  int ireduce_scatter_block(const void* src, void* dst,
                 int recvcnt, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm, MPI_Request* req);


  int type_get_name(MPI_Datatype type, char* type_name, int* resultlen);

  int type_set_name(MPI_Datatype type, const char* type_name);

  int type_extent(MPI_Datatype type, MPI_Aint* extent);

  int pack_size(int incount, MPI_Datatype datatype,
         MPI_Comm comm, int *size);

  int win_flush(int rank, MPI_Win win);

  int win_flush_local(int rank, MPI_Win win);

  int win_create(void *base, MPI_Aint size, int disp_unit, MPI_Info info,
                 MPI_Comm comm, MPI_Win *win);

  int win_free(MPI_Win *win);

  int win_lock(int lock_type, int rank, int assert, MPI_Win win);

  int win_unlock(int rank, MPI_Win win);

  int get(void *origin_addr, int origin_count, MPI_Datatype
              origin_datatype, int target_rank, MPI_Aint target_disp,
              int target_count, MPI_Datatype target_datatype, MPI_Win win);

  int put(const void *origin_addr, int origin_count, MPI_Datatype
              origin_datatype, int target_rank, MPI_Aint target_disp,
              int target_count, MPI_Datatype target_datatype, MPI_Win win);

 public:
  int op_create(MPI_User_function* user_fn, int commute, MPI_Op* op);

  int op_free(MPI_Op* op);

  int get_count(const MPI_Status* status, MPI_Datatype datatype, int* count);

  int type_dup(MPI_Datatype intype, MPI_Datatype* outtype);

  int type_set_name(MPI_Datatype id, const std::string &name);

  int type_indexed(int count, const int _blocklens_[],
               const int* _indices_,
               MPI_Datatype intype, MPI_Datatype* outtype);

  int type_hindexed(int count, const int _blocklens_[],
              const MPI_Aint* _indices_,
               MPI_Datatype intype, MPI_Datatype* outtype);


  int type_contiguous(int count, MPI_Datatype old_type, MPI_Datatype* new_type);

  int type_vector(int count, int blocklength, int stride,
              MPI_Datatype old_type,
              MPI_Datatype* new_type);

  int type_hvector(int count, int blocklength, MPI_Aint stride,
              MPI_Datatype old_type,
              MPI_Datatype* new_type);

  int type_create_struct(const int count, const int* blocklens,
              const MPI_Aint* displs,
              const MPI_Datatype* old_types,
              MPI_Datatype* newtype);

  int type_create_struct(const int count, const int* blocklens,
              const int* displs,
              const MPI_Datatype* old_types,
              MPI_Datatype* newtype);

  int type_commit(MPI_Datatype* type);

  int type_free(MPI_Datatype* type);

  mpi_type* type_from_id(MPI_Datatype id);

  void allocate_type_id(mpi_type* type);

  std::string comm_str(mpi_comm* comm);

  std::string comm_str(MPI_Comm comm);

  std::string tag_str(int tag);

  std::string src_str(int id);

  std::string src_str(mpi_comm* comm, int id);

  std::string type_str(MPI_Datatype mid);

  const char* op_str(MPI_Op op);

  mpi_comm* get_comm(MPI_Comm comm);

  mpi_group* get_group(MPI_Group grp);

  mpi_request* get_request(MPI_Request req);

  void add_comm_ptr(mpi_comm* ptr, MPI_Comm* comm);

  void erase_comm_ptr(MPI_Comm comm);

  void add_group_ptr(mpi_group* ptr, MPI_Group* grp);

  void add_group_ptr(MPI_Group grp, mpi_group* ptr);

  void erase_group_ptr(MPI_Group grp);

  void add_request_ptr(mpi_request* ptr, MPI_Request* req);

  void erase_request_ptr(MPI_Request req);

  void check_key(int key);

  void add_keyval(int key, keyval* keyval);

  keyval* get_keyval(int key);

  void finish_collective(collective_op_base* op);

 private:
  int do_wait(MPI_Request *request, MPI_Status *status);

  void finalize_wait_request(mpi_request* reqPtr, MPI_Request* request, MPI_Status* status);

  int do_type_hvector(int count, int blocklength, MPI_Aint stride,
              mpi_type* old_type,
              MPI_Datatype* new_type);

  int do_type_hindexed(int count, const int _blocklens_[],
       const MPI_Aint* _indices_,
       mpi_type* old_type, MPI_Datatype* outtype);

  void start_mpi_collective(
      collective::type_t ty,
      const void* sendbuf, void* recvbuf,
      MPI_Datatype sendtype, MPI_Datatype recvtype,
      collective_op_base* op);


  void* allocate_temp_pack_buffer(int count, mpi_type* type);

  void free_temp_pack_buffer(void* srcbuf);

  void wait_collective(collective_op_base* op);

  void free_requests(int nreqs, MPI_Request* reqs, int* inds);

  void commit_builtin_types();

  void commit_builtin_type(mpi_type* type, MPI_Datatype id);

  std::string type_label(MPI_Datatype tid);

  void start_allgather(collective_op* op);

  void start_alltoall(collective_op* op);

  void start_allreduce(collective_op* op);

  void start_barrier(collective_op* op);

  void start_bcast(collective_op* op);

  void start_gather(collective_op* op);

  void start_reduce(collective_op* op);

  void start_reduce_scatter(collective_op* op);

  void start_reduce_scatter_block(collective_op* op);

  void start_scan(collective_op* op);

  void start_scatter(collective_op* op);

  void start_allgatherv(collectivev_op* op);

  void start_alltoallv(collectivev_op* op);

  void start_gatherv(collectivev_op* op);

  void start_scatterv(collectivev_op* op);

  void finish_collective_op(collective_op_base* op_);

  void finish_vcollective_op(collective_op_base* op_);

  /* Collective operations */
  collective_op_base* start_barrier(const char* name, MPI_Comm comm);

  collective_op_base* start_bcast(MPI_Comm comm, int count, MPI_Datatype datatype,
                                  int root, void *buffer);

  collective_op_base*
  start_scatter(MPI_Comm comm, int sendcount, MPI_Datatype sendtype, int root,
           int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_scatterv(const char* name, MPI_Comm comm, const int *sendcounts, MPI_Datatype sendtype, int root,
                 const int *displs, int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_gather(MPI_Comm comm, int sendcount, MPI_Datatype sendtype, int root,
               int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_gatherv(const char* name, MPI_Comm comm, int sendcount, MPI_Datatype sendtype, int root,
          const int *recvcounts, const int *displs, MPI_Datatype recvtype,
          const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_allgather(MPI_Comm comm, int sendcount, MPI_Datatype sendtype,
            int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_allgatherv(const char* name, MPI_Comm comm, int sendcount, MPI_Datatype sendtype,
                   const int *recvcounts, const int *displs, MPI_Datatype recvtype,
                   const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_alltoall(MPI_Comm comm, int sendcount, MPI_Datatype sendtype,
                 int recvcount, MPI_Datatype recvtype, const void *sendbuf, void *recvbuf);

  collective_op_base*
  start_alltoallv(const char* name, MPI_Comm comm, const int *sendcounts, MPI_Datatype sendtype, const int *sdispls,
            const int *recvcounts, MPI_Datatype recvtype, const int *rdispls,
            const void *sendbuf,  void *recvbuf);

  collective_op_base*
  start_reduce(MPI_Comm comm, int count, MPI_Datatype type, int root,
               MPI_Op op, const void* src, void* dst);

  collective_op_base*
  start_allreduce(MPI_Comm comm, int count, MPI_Datatype type,
               MPI_Op op, const void* src, void* dst);

  collective_op_base*
  start_allreduce(mpi_comm* commPtr, int count, MPI_Datatype type,
               MPI_Op op, const void* src, void* dst);

  collective_op_base*
  start_reduce_scatter(MPI_Comm comm, const int* recvcounts, MPI_Datatype type,
                       MPI_Op op, const void* src, void* dst);

  collective_op_base*
  start_reduce_scatter_block(MPI_Comm comm, int count, MPI_Datatype type,
                             MPI_Op op, const void* src, void* dst);

  collective_op_base*
  start_scan(MPI_Comm comm, int count, MPI_Datatype type,
             MPI_Op op, const void* src, void* dst);

  void do_start(MPI_Request req);

  void add_immediate_collective(collective_op_base* op, MPI_Request* req);

  bool test(MPI_Request *request, MPI_Status *status);

  int type_size(MPI_Datatype type){
    int ret;
    type_size(type, &ret);
    return ret;
  }

  reduce_fxn get_collective_function(collective_op_base* op);

  void check_init();

  mpi_request* do_isend(const void *buf, int count, MPI_Datatype datatype, int dest,
                        int tag, MPI_Comm comm);
  int do_recv(void *buf, int count, MPI_Datatype datatype, int source,
            int tag, MPI_Comm comm, MPI_Status* status);

  int do_isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
            MPI_Comm comm, MPI_Request *request, bool print);

 private:
  friend class mpi_comm_factory;

  mpi_queue* queue_;

  MPI_Datatype next_type_id_;

  static const MPI_Op first_custom_op_id = 1000;
  MPI_Op next_op_id_;

  static sstmac::sw::ftq_tag mpi_tag;

  mpi_comm_factory comm_factory_;

  int iprobe_delay_us_;
  int test_delay_us_;

  enum {
    is_fresh, is_initialized, is_finalizing, is_finalized
  } status_;

  bool crossed_comm_world_barrier_;

  mpi_comm* worldcomm_;
  mpi_comm* selfcomm_;

  typedef std::map<MPI_Datatype, mpi_type*> type_map;
  type_map known_types_;

  typedef std::unordered_map<MPI_Op, MPI_User_function*> op_map;
  op_map custom_ops_;

  typedef std::unordered_map<MPI_Comm, mpi_comm*> comm_ptr_map;
  comm_ptr_map comm_map_;
  typedef std::unordered_map<MPI_Group, mpi_group*> group_ptr_map;
  group_ptr_map grp_map_;
  MPI_Group group_counter_;

  typedef std::unordered_map<MPI_Request, mpi_request*> req_ptr_map;
  req_ptr_map req_map_;
  MPI_Request req_counter_;

  std::unordered_map<int, keyval*> keyvals_;

  bool generate_ids_;

#if SSTMAC_COMM_SYNC_STATS
 public:
  void collect_sync_delays(double wait_start, message* msg) override;

  void start_collective_sync_delays() override;

 private:
  void set_new_mpi_call(MPI_function func){
    current_call_.ID = func;
    current_call_.sync = sstmac::timestamp();
    current_call_.start = now();
  }

  void finish_last_mpi_call(MPI_function func, bool dumpThis = true);

 private:
  double last_collection_;

  bool dump_comm_times_;

  MPI_Call current_call_;

  struct mpi_sync_timing_stats {
    sstmac::timestamp nonSync;
    sstmac::timestamp sync;
  };

  std::map<MPI_function, mpi_sync_timing_stats> mpi_calls_;
#endif

};

mpi_api* sstmac_mpi();

}

#define _start_mpi_call_(fxn) \
  SSTMACBacktrace(fxn); \
  sstmac::sw::ftq_scope scope(os_->active_thread(), mpi_tag); \
  start_api_call()

#if SSTMAC_COMM_SYNC_STATS
  #define start_mpi_call(fxn) \
    _start_mpi_call_(fxn); \
    set_new_mpi_call(Call_ID_##fxn)
  #define finish_mpi_call(fxn) \
    finish_last_mpi_call(Call_ID_##fxn); \
    end_api_call()
#else
  #define start_mpi_call(fxn) _start_mpi_call_(fxn)
  #define start_wait_call(fxn,...) _start_mpi_call_(fxn)
  #define finish_mpi_call(fxn) end_api_call()
  #define finish_Impi_call(fxn) end_api_call()
#endif

#define mpi_api_debug(flags, ...) \
  mpi_debug(comm_world()->rank(), flags, __VA_ARGS__)

#define mpi_api_cond_debug(flags, cond, ...) \
  mpi_cond_debug(comm_world()->rank(), flags, cond, __VA_ARGS__)


#endif
