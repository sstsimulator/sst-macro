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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPI_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPI_H_INCLUDED

#include <sstmac/common/messages/payload.h>

#include <sstmac/software/libraries/library.h>
#include <sstmac/software/api/api.h>

#include <sumi-mpi/sstmac_mpi.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_types/mpi_type_fwd.h>

#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_status.h>

#include <sumi-mpi/mpi_comm/mpi_comm_factory.h>
#include <sumi-mpi/mpi_debug.h>

#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/pmi.h>
#include <sstmac/software/process/backtrace.h>

#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/unordered.h>
#include <sprockit/factories/factory.h>

#include <sstmac/libraries/sumi/sumi_transport.h>

namespace sumi {

using sstmac::sw::key;
using sstmac::sw::software_id;
using sstmac::sw::operating_system;

class mpi_api :
  public sumi_transport
{
  /// Nested classes to take care of persistent communications.
 public:
  class persistent;
 private:
  class persistent_send;
  class persistent_recv;

 public:
  static key::category default_key_category;
  static key::category poll_key_category;
  static key::category memcpy_key_category;

  /// Build a new mpiapi.
  mpi_api();

  virtual void
  finalize_init();

  static void
  delete_statics();

 public:
  virtual
  ~mpi_api();

  void
  init_param1(const software_id& id);

  void
  init_os(operating_system* os);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  mpi_queue*
  queue() {
    return queue_;
  }

  /*
   * Methods exposing MPI calls and other key methods to
   * derived objects.
   */
  /// Get the world communicator.
  mpi_comm*
  comm_world() const {
    return worldcomm_;
  }

  /// Get the self communicator.
  mpi_comm*
  comm_self() const {
    return selfcomm_;
  }

  /* Utility functions to mirror MPI API functionality */

  /// The rank of this node in a communicator.
  /// The size of a communicator.
  int comm_rank(MPI_Comm comm, int* rank);

  /// The size of a communicator.
  int comm_size(MPI_Comm comm, int* size);

  /// Get type size.
  int
  type_size(MPI_Datatype type);

  /* Set up and tear down */

  /// Test whether MPI has bee initialized.
  int
  initialized(int* flag){
    *flag = (status_ == is_initialized);
    return MPI_SUCCESS;
  }

  int
  finalized(int* flag){
    *flag = (status_ == is_finalized);
    return MPI_SUCCESS;
  }

  /// Initialize MPI.  Must be the first MPI call made other than
  /// initialized or finalized.
  int
  do_init(int* argc, char*** argv);

  /// Finalize MPI.  Must be the last MPI call other than
  /// initialized or finalized.
  int
  do_finalize();

  /// Get current time.
  double
  wtime();

  /* Create and destroy communicators. */
  /// Split a communicator.  This one is a little weird.
  int
  comm_split(MPI_Comm incomm, int color, int key,
             MPI_Comm* outcomm);

  /// Duplicate a communicator.
  int
  comm_dup(MPI_Comm input, MPI_Comm* output);

  /// Create a communicator containing a subset of an existing comm.
  int
  comm_create(MPI_Comm input, MPI_Group group,
              MPI_Comm* output);

  int
  cart_create(MPI_Comm comm_old, int ndims, const int dims[],
              const int periods[], int reorder, MPI_Comm *comm_cart);

  int
  cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[],
                   int coords[]);

  int
  cartdim_get(MPI_Comm comm, int *ndims);

  int
  cart_rank(MPI_Comm comm, const int coords[], int *rank);

  int
  cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source,
             int *rank_dest);

  int
  cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[]);

  /// Destroy a communicator.  This is currently a noop, but should later
  /// mark the communicator invalid so erroneous program behavior can be
  /// detected.
  int
  comm_free(MPI_Comm* input);

  int
  group_incl(int* ranks,
             int num_ranks,
             MPI_Group oldgrp,
             MPI_Group* newgrp);

  /* Basic point-to-point operations. */
  int send(const void *buf, int count,
           MPI_Datatype datatype, int dest, int tag,
           MPI_Comm comm);

  int isend(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
            MPI_Comm comm, MPI_Request *request);

  int recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
           MPI_Comm comm, MPI_Status *status);

  int irecv(void *buf, int count, MPI_Datatype datatype, int source,
            int tag, MPI_Comm comm, MPI_Request *request);

  /* Completion of outstanding requests */
  int
  wait(MPI_Request *request, MPI_Status *status);

  int
  waitall(int count, MPI_Request array_of_requests[],
          MPI_Status array_of_statuses[]);

  int
  waitany(int count, MPI_Request array_of_requests[], int *indx,
          MPI_Status *status);

  int
  waitsome(int incount, MPI_Request array_of_requests[],
           int *outcount, int array_of_indices[],
           MPI_Status array_of_statuses[]);

  int
  test(MPI_Request *request, int *flag, MPI_Status *status);

  int
  testall(int count, MPI_Request array_of_requests[], int *flag,
          MPI_Status array_of_statuses[]);

  int
  testany(int count, MPI_Request array_of_requests[], int *indx,
          int *flag, MPI_Status *status);

  int
  testsome(int incount, MPI_Request array_of_requests[], int *outcount,
           int array_of_indices[], MPI_Status array_of_statuses[]);

  int
  probe(int source, int tag, MPI_Comm comm,
         MPI_Status *status);

  int
  iprobe(int source, int tag, MPI_Comm comm, int* flag,
         MPI_Status *status);

  /* Collective operations */
  /// Set a barrier.
  /// \return the time at which this node was released from the barrier.
  int
  barrier(MPI_Comm comm);

  int
  bcast(int count, MPI_Datatype datatype, int root,
        MPI_Comm comm);

  int
  bcast(void *buffer, int count, MPI_Datatype datatype, int root,
        MPI_Comm comm);

  int
  scatter(int sendcount, MPI_Datatype sendtype,
          int recvcount, MPI_Datatype recvtype, int root,
          MPI_Comm comm);

  int
  scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
           void *recvbuf, int recvcount, MPI_Datatype recvtype, int root,
           MPI_Comm comm);

  int
  scatterv(const int *sendcounts,
           MPI_Datatype sendtype, int recvcount,
           MPI_Datatype recvtype,
           int root, MPI_Comm comm);

  int
  scatterv(const void *sendbuf, const int *sendcounts, const int *displs,
           MPI_Datatype sendtype, void *recvbuf, int recvcount,
           MPI_Datatype recvtype,
           int root, MPI_Comm comm);

  int
  gather(int sendcount, MPI_Datatype sendtype,
         int recvcount, MPI_Datatype recvtype,
         int root, MPI_Comm comm);

  int
  gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
         void *recvbuf, int recvcount, MPI_Datatype recvtype,
         int root, MPI_Comm comm);
  int
  gatherv(int sendcount, MPI_Datatype sendtype,
         const int *recvcounts,
         MPI_Datatype recvtype, int root, MPI_Comm comm);

  int
  gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
          void *recvbuf, const int *recvcounts, const int *displs,
          MPI_Datatype recvtype, int root, MPI_Comm comm);

  int
  allgather(int sendcount, MPI_Datatype sendtype,
            int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int
  allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
            void *recvbuf, int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int
  allgatherv(int sendcount, MPI_Datatype sendtype,
             const int *recvcounts,
             MPI_Datatype recvtype, MPI_Comm comm);

  int
  allgatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
             void *recvbuf, const int *recvcounts, const int *displs,
             MPI_Datatype recvtype, MPI_Comm comm);

  int
  alltoall(int sendcount, MPI_Datatype sendtype,
           int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);

  int
  alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
            void *recvbuf, int recvcount, MPI_Datatype recvtype,
            MPI_Comm comm);
  int
  alltoallv(const int *sendcounts,
            MPI_Datatype sendtype,
            const int *recvcounts,
            MPI_Datatype recvtype,
            MPI_Comm comm);

  int
  alltoallv(const void *sendbuf, const int *sendcounts,
            const int *sdispls, MPI_Datatype sendtype, void *recvbuf,
            const int *recvcounts, const int *rdispls, MPI_Datatype recvtype,
            MPI_Comm comm);

  /// Reduce data from all nodes to all nodes and give all nodes access
  /// to the payload posted by each node.  The result pointer will point to
  /// an object of type mpicollpayload.
  int
  reduce(int count, MPI_Datatype type, MPI_Op op, int root,
            MPI_Comm comm);

  /// Reduce data from all nodes to all nodes and give all nodes access
  /// to the payload posted by each node.  The result pointer will point to
  /// an object of type mpicollpayload.
  int
  reduce(const void* src, void* dst,
            int count, MPI_Datatype type, MPI_Op op, int root,
            MPI_Comm comm);

  /// Reduce data from all nodes to all nodes and give all nodes access
  /// to the payload posted by each node.  The result pointer will point to
  /// an object of type mpicollpayload.
  int
  allreduce(int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm);

  /// Reduce data from all nodes to all nodes and give all nodes access
  /// to the payload posted by each node.  The result pointer will point to
  /// an object of type mpicollpayload.
  int
  allreduce(const void* src, void* dst,
            int count, MPI_Datatype type, MPI_Op op,
            MPI_Comm comm);

  /// Reduce data and scatter results.
  /// All nodes will get a pointer to all payload.
  int
  reduce_scatter(int* recvcnts, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm);

  /// Reduce data and scatter results.
  /// All nodes will get a pointer to all payload.
  int
  reduce_scatter(const void* src, void* dst,
                 int* recvcnts, MPI_Datatype type,
                 MPI_Op op, MPI_Comm comm);

  /// Compute partial reductions on data.
  int
  scan(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm);

  /// Compute partial reductions on data and share data.
  /// Each node will have as a result its own payload and that of
  /// all lower-ranked nodes
  int
  scan(const void* src, void* dst,
      int count, MPI_Datatype type, MPI_Op op,
       MPI_Comm comm);


 public:
  /// Duplicate a type.  Easy enough.
  int
  type_dup(MPI_Datatype intype, MPI_Datatype* outtype);

  int
  type_set_name(MPI_Datatype id, const std::string &name);

  /// Create an indexed type.  Yes, lets recreate C++ with MPI function calls.
  int
  type_indexed(int count, int _blocklens_[], const int* _indices_,
               MPI_Datatype intype, MPI_Datatype* outtype, bool in_elem, int comb);

  /// Creates a contiguous datatype
  int
  type_contiguous(int count, MPI_Datatype old_type, MPI_Datatype* new_type);

  /// Creates a vector (strided) datatype
  int
  type_vector(int count, int blocklength, int stride,
              MPI_Datatype old_type,
              MPI_Datatype* new_type,
              bool stride_in_elem);

  /// Creates a struct datatype
  int
  type_struct(const int count, const int* blocklens,
              const int* indices,
              const MPI_Datatype* old_types,
              MPI_Datatype* newtype);

  /// A datatype object has to be committed before use in communication.
  int
  type_commit(MPI_Datatype type);

  /// Mark datatype for deallocation.
  int
  type_free(MPI_Datatype type);

  mpi_type*
  type_from_id(MPI_Datatype id);

  void
  allocate_type_id(mpi_type* type);

  std::string
  comm_str(mpi_comm* comm);

  std::string
  comm_str(MPI_Comm comm);

  std::string
  tag_str(int tag);

  std::string
  src_str(int id);

  std::string
  src_str(mpi_comm* comm, int id);

  std::string
  type_str(MPI_Datatype mid);

  const char*
  op_str(MPI_Op op);

  mpi_comm*
  get_comm(MPI_Comm comm);

  mpi_group*
  get_group(MPI_Group grp);

  mpi_request*
  get_request(MPI_Request req);

  MPI_Comm
  add_comm_ptr(mpi_comm* ptr);

  void
  erase_comm_ptr(MPI_Comm comm);

  MPI_Group
  add_group_ptr(mpi_group* ptr);

  void
  add_group_ptr(MPI_Group grp, mpi_group* ptr);

  void
  erase_group_ptr(MPI_Group grp);

  MPI_Request
  add_request_ptr(mpi_request* ptr);

  void
  erase_request_ptr(MPI_Request req);

  void
  add_comm_grp(MPI_Comm comm, MPI_Group grp);

  MPI_Group
  get_comm_grp(MPI_Comm comm);

  void
  check_key(int key);

  void
  add_keyval(int key, keyval* keyval);

  keyval*
  get_keyval(int key);

 private:
  int
  do_wait(MPI_Request *request, MPI_Status *status);

  void
  validate_mpi_collective(const char* name, MPI_Datatype sendtype, MPI_Datatype recvtype);

  void collective_progress_loop(sumi::collective::type_t ty, int tag);

  void
  free_requests(int nreqs,
    MPI_Request* reqs,
    int* inds);

  void
  precommit_types();

  void
  precommit_type(mpi_type* type, MPI_Datatype id);

  std::string
  type_label(MPI_Datatype tid);

  /**
   * @brief start_allgather
   * @param sendbuf
   * @param recvbuf
   * @param count
   * @param type
   * @param comm
   * @return A unique tag identifying the collective
   */
  int start_allgather(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, MPI_Comm comm);

  int start_alltoall(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, MPI_Comm comm);

  int start_allreduce(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm);

  int start_barrier(MPI_Comm comm);

  int start_bcast(void *buffer, int count, MPI_Datatype type, int root, MPI_Comm comm);

  int start_gather(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, int root, MPI_Comm comm);

  int start_reduce(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm);

  int start_reduce_scatter(const void *src, void *dst, int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm);

  int start_scan(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm);

  int start_scatter(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, int root, MPI_Comm comm);

  int start_allgatherv(const void *sendbuf, void *recvbuf, int sendcount, const int *recvcounts, const int *displs, MPI_Datatype type, MPI_Comm comm);

  int start_alltoallv(const void *sendbuf,  void *recvbuf, const int *sendcounts, const int *sdispls, const int *recvcounts, const int *rdispls, MPI_Datatype type, MPI_Comm comm);

  int start_gatherv(const void *sendbuf, void *recvbuf, int sendcount, const int *recvcounts, const int *displs, MPI_Datatype type, int root, MPI_Comm comm);

  int start_scatterv(const void *sendbuf, void *recvbuf, const int* sendcounts, const int *displs, int recvcount, MPI_Datatype type, int root, MPI_Comm comm);

  bool test(MPI_Request *request, MPI_Status *status);

 private:
  software_id id_;

  /// The MPI server.
  mpi_queue* queue_;

  //// My MPI index in the world.
  int rank_;

  MPI_Datatype next_type_id_;

  /// The builder for mpi communicators.
  mpi_comm_factory* comm_factory_;

  /// Ensure that collective operations are given unique tags
  /// to protect shared back-end data (magic payload info).

  /// The state of this object (initialized or not).
  enum {
    is_fresh, is_initialized, is_finalizing, is_finalized
  } status_;

  /// MPI_COMM_WORLD.
  mpi_comm* worldcomm_;

  /// MPI_COMM_SELF.
  mpi_comm* selfcomm_;

  //----------------------------------------------------------------
  // --- MPI Derived Datatype
  // --- Presently, payloads won't work with derived datatypes
  //----------------------------------------------------------------
  typedef std::map<MPI_Datatype, mpi_type*> type_map;
  type_map known_types_;

  typedef spkt_unordered_map<MPI_Comm, mpi_comm*> comm_ptr_map;
  comm_ptr_map comm_map_;
  typedef spkt_unordered_map<MPI_Group, mpi_group*> group_ptr_map;
  group_ptr_map grp_map_;
  MPI_Group group_counter_;

  typedef spkt_unordered_map<MPI_Comm, MPI_Group> comm_grp_map;
  comm_grp_map comm_grp_map_;

  typedef spkt_unordered_map<MPI_Request, mpi_request*> req_ptr_map;
  req_ptr_map req_map_;
  MPI_Request req_counter_;

  spkt_unordered_map<int, keyval*> keyvals_;

};

mpi_api*
sstmac_mpi();

#define mpi_api_debug(flags, ...) \
  mpi_debug(worldcomm_->rank(), flags, __VA_ARGS__)

}

#endif

