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

#include <sstmac/libraries/mpi/sstmac_mpi_integers.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_info.h>
#include <sstmac/libraries/mpi/rma/mpi_window.h>

#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_status.h>

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategy.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_factory.h>
#include <sstmac/libraries/mpi/mpi_debug.h>

#include <sprockit/factories/factory.h>

#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/pmi.h>

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_fwd.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

#include <sprockit/unordered.h>

namespace sstmac {
namespace sw {


class mpi_api :
  public api,
  public process_manager
{
  /// This friendship declaration can probably be eliminated later.
  friend class mpilaunch;

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

  enum LOCK_TYPE {
    mpi_lock_shared_ = 0, mpi_lock_exclusive_
  };
  enum WIN_FLAGS {
    mpi_flag_none_ = 0, mpi_win_mode_nocheck_
  };

  /// Build a new mpiapi.
  mpi_api();

  int
  save_payload(const payload::const_ptr& load) {
    int ret = has_saved_payload();
    saved_payload_ = load;
    return ret;
  }

  payload::const_ptr
  saved_payload() {
    return saved_payload_;
  }

  bool
  has_saved_payload() {
    int ret = (saved_payload_ != payload::null());
    return ret;
  }

  virtual void
  finalize_init();

  static void
  delete_statics();

 public:
  /// Adieu.
  virtual
  ~mpi_api();

  void
  init_param1(const software_id& id);

  void
  init_os(operating_system* os);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  timestamp starttime_;
  timestamp runtime_;

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
  inline mpi_id
  comm_rank(mpi_comm* comm) {
    return comm->rank();
  }

  /// The size of a communicator.
  inline mpi_id
  comm_size(mpi_comm* comm) {
    return comm->size();
  }

  void
  attr_put(mpi_comm* comm, keyval* k, void* val);

  void
  attr_get(mpi_comm* comm, keyval* k, void* val, int *flag);

  timestamp
  mpi_cart_create(mpi_comm* &comm, int ndims, int *dims, int *periods,
                  int reorder, mpi_comm* &outcomm);

  timestamp
  mpi_cart_get(mpi_comm* &comm, int maxdims, int *dims, int *periods,
               int *coords);

  timestamp
  mpi_cartdim_get(mpi_comm* &comm, int *ndims);

  timestamp
  mpi_cart_rank(mpi_comm* comm, int *coords, int *rank);

  timestamp
  mpi_cart_shift(mpi_comm* comm, int direction, int displ, int *source,
                 int *dest);

  timestamp
  mpi_cart_coords(mpi_comm* comm, int rank, int maxdims, int *coords);

  /// Get the number of elements received assuming a specific type.
  int
  count(mpi_status* status, mpi_type_id type);

  /// Split a communicator.  This one is a little weird.
  timestamp
  comm_split(mpi_comm* incomm, int color, int key,
             mpi_comm* &outcomm);

  /// Get type size.
  int
  type_size(mpi_type_id type);

  /* Set up and tear down */

  /// Test whether MPI has bee initialized.
  bool
  initialized();

  /// Test whether MPI has been finalized.
  bool
  finalized();

  /// Initialize MPI.  Must be the first MPI call made other than
  /// initialized or finalized.
  timestamp
  init();

  /// Finalize MPI.  Must be the last MPI call other than
  /// initialized or finalized.
  timestamp
  finalize();

  /// Get current time.
  timestamp
  wtime();

  /* Create and destroy communicators. */

  /// Duplicate a communicator.
  timestamp
  comm_dup(mpi_comm*input, mpi_comm* &output);

  /// Create a communicator containing a subset of an existing comm.
  timestamp
  comm_create(mpi_comm*input, mpi_group* group,
              mpi_comm* &output);

  /// Destroy a communicator.  This is currently a noop, but should later
  /// mark the communicator invalid so erroneous program behavior can be
  /// detected.
  timestamp
  comm_free(mpi_comm* input);

  timestamp
  group_incl(int* ranks,
             int num_ranks,
             mpi_group* oldgrp,
             mpi_group*& newgrp);

  /* Basic point-to-point operations. */

  /// Blocking send.
  /// \return time at which the (imaginary) input buffer can be reused.
  timestamp
  send(int count, mpi_type_id type, mpi_id target,
       mpi_tag tag, mpi_comm* comm,
       const payload::const_ptr& content = payload::null());

  /// Blocking send using a user-defined buffer (see buffer_attach).
  /// \return time at which it is safe to use the (hypothetical) input array.
  timestamp
  bsend(int count, mpi_type_id type, mpi_id target,
        mpi_tag tag, mpi_comm* comm,
       const payload::const_ptr& content = payload::null());

  /// Blocking syncronous send.
  /// \return time when a matching recv (or irecv) has acknowledged receipt.
  timestamp
  ssend(int count, mpi_type_id type, mpi_id target,
        mpi_tag tag, mpi_comm* comm,
       const payload::const_ptr& content = payload::null());

  /// Ready send (as in "ready or not, here comes the data").
  /// \return time when it is safe to use the input array.
  timestamp
  rsend(int count, mpi_type_id type, mpi_id target,
        mpi_tag tag, mpi_comm* comm,
       const payload::const_ptr& content = payload::null());

  /// Nonblocking send.
  /// wait for this request has the same completion as send.
  /// \return time when the request has been registered (likely current time).
  timestamp
  isend(int count, mpi_type_id type, mpi_id target,
        mpi_tag tag, mpi_comm* comm, mpi_request* &req,
        const payload::const_ptr& content = payload::null());

  /// Nonblocking user-buffered send.
  /// wait for this request has the same completion as bsend.
  /// \return time when the request has been registered (likely current time).
  timestamp
  ibsend(int count, mpi_type_id type, mpi_id target,
         mpi_tag tag, mpi_comm* comm, mpi_request* &req,
         const payload::const_ptr& content = payload::null());

  /// Nonblocking synchronous send.
  /// wait for this request has the same completion as ssend.
  timestamp
  issend(int count, mpi_type_id type, mpi_id target,
         mpi_tag tag, mpi_comm* comm, mpi_request* &req,
         const payload::const_ptr& content = payload::null());

  /// Nonblocking ready send.
  /// wait for this request has the same completion as rsend.
  timestamp
  irsend(int count, mpi_type_id type, mpi_id target,
         mpi_tag tag, mpi_comm* comm, mpi_request* &req,
         const payload::const_ptr& content = payload::null());

  /// Blocking receive.
  /// Note that the message content (may be null) is available
  /// via stat.content().
  /// \return time at which the input data is available for use.
  timestamp
  recv(int count, mpi_type_id type, mpi_id source,
       mpi_tag tag, mpi_comm* comm, mpi_status* stat);

  /// Asynchronous receive.
  /// wait for this request has the same completion as recv.
  /// \return time at which the receive request has been registered.
  timestamp
  irecv(int count, mpi_type_id type, mpi_id source,
        mpi_tag tag, mpi_comm* comm, mpi_request* &req);

  /// Send/receieve exchange.
  /// \return time at which the request has been completed.
  timestamp
  sendrecv(int sendcount, mpi_type_id sendtype, mpi_id target,
           mpi_tag sendtag, int recvcount, mpi_type_id recvtype,
           mpi_id source, mpi_tag recvtag, mpi_comm* comm,
           mpi_status* stat,
           const payload::const_ptr& content = payload::null());

  /// Send and receive using a single buffer.
  timestamp
  sendrecv_replace(int count, mpi_type_id type, mpi_id dest,
                   mpi_tag sendtag, mpi_id source, mpi_tag recvtag,
                   mpi_comm* comm, mpi_status* stat);

  /* Stepwise point-to-point (not implemented, but
   * should be fairly straight-forward to complete). */

  /// Initiate a send.
  timestamp
  send_init(int count, mpi_type_id type, mpi_id target,
            mpi_tag tag, mpi_comm* comm, mpi_request* &req,
            void* buf = NULL);

  /// Initiate a buffered send.
  timestamp
  bsend_init(int count, mpi_type_id type, mpi_id target,
             mpi_tag tag, mpi_comm* comm, mpi_request* &req,
             void* buf = NULL);

  /// Initiate a synchronous send.
  timestamp
  ssend_init(int count, mpi_type_id type, mpi_id target,
             mpi_tag tag, mpi_comm* comm, mpi_request* &req,
             void* buf = NULL);

  /// Initiate a ready send.
  timestamp
  rsend_init(int count, mpi_type_id type, mpi_id target,
             mpi_tag tag, mpi_comm* comm, mpi_request* &req,
             void* buf = NULL);

  /// Initiate a receive.
  timestamp
  recv_init(int count, mpi_type_id type, mpi_id target,
            mpi_tag tag, mpi_comm* comm, mpi_request* &req);

  /// Start a previously initiated request.
  timestamp
  start(mpi_request* req);

  /// Start a collection of previously initiated requests.
  timestamp
  startall(std::vector<mpi_request*> &requests);

  /// Cancel a request that had been previously initiated.
  timestamp
  cancel(mpi_request* req);

  /* Completion of outstanding requests */

  timestamp
  wait(mpi_request** req);

  timestamp
  wait(mpi_request** req, mpi_status* stat);

  timestamp
  waitall(std::vector<mpi_request*>& req);

  timestamp
  waitall(std::vector<mpi_request*>& req,
    std::vector<mpi_status>& stats);

  timestamp
  waitany(std::vector<mpi_request*>& reqs, int& index);

  timestamp
  waitany(std::vector<mpi_request*>& reqs, int& index,
    mpi_status* stat);

  timestamp
  waitany(std::vector<mpi_request*>& reqs, int& index,
    timestamp timeout);

  timestamp
  waitany(std::vector<mpi_request*>& reqs, int& index,
    mpi_status* stat, timestamp timeout);

  timestamp
  waitsome(std::vector<mpi_request*>& req,
    std::vector<int>& indices);

  timestamp
  waitsome(std::vector<mpi_request*>& req,
    std::vector<int>& indices,
    std::vector<mpi_status>& stats);

  timestamp
  test(mpi_request** req, bool& flag);

  timestamp
  test(mpi_request** req, bool& flag, mpi_status* status);

  timestamp
  testall(std::vector<mpi_request*>& req, bool& flag);

  timestamp
  testall(std::vector<mpi_request*>& req, bool& flag,
    std::vector<mpi_status>& stats);

  timestamp
  testany(std::vector<mpi_request*>& req, int& index, bool& flag);

  timestamp
  testany(std::vector<mpi_request*>& req, int& index, bool& flag,
    mpi_status* stat);

  timestamp
  testsome(std::vector<mpi_request*>& req,
          std::vector<int>& indices);

  timestamp
  testsome(std::vector<mpi_request*>& req,
          std::vector<int>& indices,
          std::vector<mpi_status>& stats);

  timestamp
  probe(mpi_id source, mpi_tag tag, mpi_comm* comm, mpi_status* stat);

  /// Non-blocking probe for messages.
  /// \return current time when the call completes.
  timestamp
  iprobe(mpi_id source, mpi_tag tag, mpi_comm* comm,
         bool& flag, mpi_status* stat);

  /* Mucking with MPI state */

  /// Attach a user-defined buffer for *bsend operations.
  timestamp
  buffer_attach(int bytes);

  /// Detach the user-defined buffer.
  /// \param bytes is the previously assigned buffer size.
  timestamp
  buffer_detach(int &bytes);

  /* Collective operations */

  /// Set a barrier.
  /// \return the time at which this node was released from the barrier.
  timestamp
  barrier(mpi_comm* comm);

  /// Broadcast data with no payload.
  timestamp
  bcast(int count, mpi_type_id type, mpi_id root,
        mpi_comm* comm);

  /// Broadcast data and send/receive payload.
  /// On root, the content will be unchanged.
  /// Other nodes will get a copy of root's payload.
  timestamp
  bcast(int count, mpi_type_id type, mpi_id root,
        mpi_comm* comm, payload::const_ptr &content);

  /// Scatter data uniformly.
  timestamp
  scatter(int sendcount, mpi_type_id sendtype, int recvcount,
          mpi_type_id recvtype, mpi_id root, mpi_comm* comm);

  /// Scatter data uniformly and send payload.
  /// The content at root will be sent to all nodes.
  /// The 'result' pointer will be updated on all nodes to
  /// point to a copy of root's content.
  timestamp
  scatter(int sendcount, mpi_type_id sendtype, int recvcount,
          mpi_type_id recvtype, mpi_id root, mpi_comm* comm,
          const std::vector<payload::const_ptr> &cc,
          payload::const_ptr& result, bool usingpayload = true);

  /// Scatter data non-uniformly.
  timestamp
  scatterv(const std::vector<int> &sendcounts, mpi_type_id sendtype,
           int recvcount, mpi_type_id recvtype, mpi_id root,
           mpi_comm* comm);

  /// Scatter data non-uniformly.
  /// The content at root will be sent to all nodes.
  /// The 'result' pointer will be updated on all nodes to
  /// point to a copy of root's content.
  timestamp
  scatterv(const std::vector<int> &sendcounts, mpi_type_id sendtype,
           int recvcount, mpi_type_id recvtype, mpi_id root,
           mpi_comm* comm,
           const std::vector<payload::const_ptr>& content,
           payload::const_ptr& result, bool usingpayload = true);

  /// Gather uniformly distributed data.
  timestamp
  gather(int sendcount, mpi_type_id sendtype, int recvcount,
         mpi_type_id recvtype, mpi_id root, mpi_comm* comm);

  /// Gather uniformly distributed data and payload.
  /// The 'result' at root will have pointers to copies of
  /// all node's content.
  timestamp
  gather(int sendcount, mpi_type_id sendtype, int recvcount,
         mpi_type_id recvtype, mpi_id root, mpi_comm* comm,
         const payload::const_ptr& content,
         std::vector<payload::const_ptr> &cc);

  /// Gather non-uniformly distributed data.
  timestamp
  gatherv(int sendcount, mpi_type_id sendtype,
          const std::vector<int> &recvcnts, mpi_type_id recvtype,
          mpi_id root, mpi_comm* comm);

  /// Gather non-uniformly distributed data and payload.
  /// The 'result' at root will have pointers to copies of
  /// all node's content.
  timestamp
  gatherv(int sendcount, mpi_type_id sendtype,
          const std::vector<int> &recvcnts, mpi_type_id recvtype,
          mpi_id root, mpi_comm* comm,
          const payload::const_ptr& content,
          std::vector<payload::const_ptr> &result);

  /// Gather uniformly distributed data and make it available to all nodes.
  timestamp
  allgather(int sendcount, mpi_type_id sendtype, int recvcount,
            mpi_type_id recvtype, mpi_comm* comm);

  /// Gather uniformly distributed data and make it available to all nodes.
  timestamp
  allgather(int sendcount, mpi_type_id sendtype, int recvcount,
            mpi_type_id recvtype, mpi_comm* comm,
            const payload::const_ptr& content,
            std::vector<payload::const_ptr> &result);

  timestamp
  run_allgather(int sendcount, mpi_type_id sendtype, int recvcount,
            mpi_type_id recvtype, mpi_comm* comm,
            const payload::const_ptr& content,
            std::vector<payload::const_ptr> &result);


  /// Gather non-uniformly distributed data onto all nodes.
  timestamp
  allgatherv(int sendcount, mpi_type_id sendtype,
             const std::vector<int> &recvcnts, mpi_type_id recvtype,
             mpi_comm* comm);

  /// Gather non-uniformly distributed data onto all nodes.
  timestamp
  allgatherv(int sendcount, mpi_type_id sendtype,
             const std::vector<int> &recvcnts, mpi_type_id recvtype,
             mpi_comm* comm, const payload::const_ptr& content,
             std::vector<payload::const_ptr> &result);

  /// Send uniformly distributed data from all to all processes.
  timestamp
  alltoall(int sendcount, mpi_type_id sendtype, int recvcount,
           mpi_type_id recvtype, mpi_comm* comm);

  /// Send uniformly distributed data from all to all processes.
  timestamp
  alltoall(int sendcount, mpi_type_id sendtype, int recvcount,
           mpi_type_id recvtype, mpi_comm* comm,
           const std::vector<payload::const_ptr>& content,
           std::vector<payload::const_ptr>& result);

  /// Send non-uniformly distributed data from all to all processors.
  timestamp
  alltoallv(const std::vector<int> &sendcnts, mpi_type_id sendtype,
            const std::vector<int> &recvcnts, mpi_type_id recvtype,
            mpi_comm* comm);

  /// Send non-uniformly distributed data from all to all processors.
  timestamp
  alltoallv(const std::vector<int> &sendcnts, mpi_type_id sendtype,
            const std::vector<int> &recvcnts, mpi_type_id recvtype,
            mpi_comm* comm,
            const std::vector<payload::const_ptr>& content,
            std::vector<payload::const_ptr>& result);

  /// Reduce data from all nodes onto the root node.
  timestamp
  reduce(int count, mpi_type_id type, mpi_op* op,
         mpi_id root, mpi_comm* comm);

  /// Reduce data from all nodes onto the root node and give root node
  /// access to everybody's payload.
  /// On root node, the result pointer will be rewritten to point to
  /// an object of type mpicollpayload.  On other nodes, this
  /// pointer will be set to null.
  timestamp
  reduce(int count, mpi_type_id type, mpi_op* op,
         mpi_id root, mpi_comm* comm,
         const payload::const_ptr& content,
         payload::const_ptr& result);

  /// Reduce data from all nodes to all nodes.
  timestamp
  allreduce(int count, mpi_type_id type, mpi_op* op,
            mpi_comm* comm);

  /// Reduce data from all nodes to all nodes and give all nodes access
  /// to the payload posted by each node.  The result pointer will point to
  /// an object of type mpicollpayload.
  timestamp
  allreduce(int count, mpi_type_id type, mpi_op* op,
            mpi_comm* comm, const payload::const_ptr& content,
            payload::const_ptr& result);

  /// Reduce data and scatter results.
  timestamp
  reduce_scatter(const std::vector<int> &recvcnts,
                 mpi_type_id type,
                 mpi_op* op, mpi_comm* comm);

  /// Reduce data and scatter results.
  /// All nodes will get a pointer to all payload.
  timestamp
  reduce_scatter(const std::vector<int> &recvcnts, mpi_type_id type,
                 mpi_op* op, mpi_comm* comm,
                 const payload::const_ptr& content,
                 payload::const_ptr& result);

  /// Compute partial reductions on data.
  timestamp
  scan(int count, mpi_type_id type, mpi_op* op,
       mpi_comm* comm);

  /// Compute partial reductions on data and share data.
  /// Each node will have as a result its own payload and that of
  /// all lower-ranked nodes
  timestamp
  scan(int count, mpi_type_id type, mpi_op* op,
       mpi_comm* comm,
       const payload::const_ptr& content,
       payload::const_ptr& result);


 public:
  /// Duplicate a type.  Easy enough.
  timestamp
  type_dup(mpi_type_id intype, mpi_type_id& outtype);

  timestamp
  type_set_name(mpi_type_id id, const std::string &name);

  /// Create an indexed type.  Yes, lets recreate C++ with MPI function calls.
  timestamp
  type_indexed(int count, int _blocklens_[], const std::vector<int> &_indices_,
               mpi_type_id intype, mpi_type_id& outtype, bool in_elem, int comb);

  /// Creates a contiguous datatype
  timestamp
  type_contiguous(int count, mpi_type_id old_type, mpi_type_id& new_type);

  /// Creates a vector (strided) datatype
  timestamp
  type_vector(int count, int blocklength, int stride,
              mpi_type_id old_type,
              mpi_type_id& new_type,
              bool stride_in_elem);

  /// Creates a struct datatype
  timestamp
  type_struct(const int count, const std::vector<int> &blocklens,
              const std::vector<int> &indices,
              const std::vector<mpi_type_id> &old_types,
              mpi_type_id& newtype);

  /// A datatype object has to be committed before use in communication.
  timestamp
  type_commit(mpi_type_id type);

  /// Mark datatype for deallocation.
  timestamp
  type_free(mpi_type_id type);

  /**
    Should only be called by sstmac_mpi implementation
  */
  void
  type_commit(MPI_Datatype dtype);

  /**
    Should only be called by sstmac_mpi implementation
  */
  void
  type_free(MPI_Datatype dtype);

  mpi_type*
  type_from_id(mpi_type_id id);

  mpi_op*
  created_op(unsigned long id);

  timestamp
  op_create(mpi_op::op_fxn fxn, int comm, mpi_op* &output);

  timestamp
  op_free(long id);

  //----------------------------------------------------------------
  // --- MPI-2 RMA / windows
  //----------------------------------------------------------------

  timestamp
  win_create(void* base, size_t size, int disp, mpi_info* inf,
             mpi_comm* comm, mpi_window* &win, long id);

  timestamp
  win_free(mpi_window* win);

  timestamp
  win_get(mpi_request* &req, int origin_count,
          mpi_type_id origin_datatype, mpi_id target_rank,
          MPI_Aint target_disp, int target_count,
          mpi_type_id target_datatype, mpi_window* win);

  timestamp
  win_put(mpi_request* &req, const payload::const_ptr& load,
          int target_count, mpi_type_id target_datatype,
          mpi_id target_rank, long long target_disp,
          mpi_window* win);

  timestamp
  win_accumulate(mpi_request* &req, const payload::const_ptr& load,
                 int target_count, mpi_type_id target_datatype,
                 mpi_id target_rank, long long target_disp, mpi_op*  op,
                 mpi_window* win);

  timestamp
  win_fence(mpi_window* win, bool barr,
            spkt_unordered_map<mpi_request*, payload::const_ptr> &loads);

  timestamp
  win_unlock(mpi_id rank, mpi_window* win,
             spkt_unordered_map<mpi_request*, payload::const_ptr> &loads);

  timestamp
  win_lock(LOCK_TYPE t, mpi_id rank, WIN_FLAGS flag,
           mpi_window* win);

  void
  allocate_type_id(mpi_type* type);

  std::string
  comm_str(mpi_comm* comm);

  std::string
  comm_str(mpi_comm_id comm);

  std::string
  tag_str(mpi_tag tag);

  std::string
  src_str(mpi_id id);

  std::string
  src_str(mpi_comm* comm, mpi_id id);

  std::string
  type_str(mpi_type_id mid);

  std::string
  op_str(mpi_op* op);

  mpi_comm*
  get_comm(MPI_Comm comm);

  mpi_group*
  get_group(MPI_Group grp);

  mpi_info*
  get_info(MPI_Info inf);

  mpi_window*
  get_window(MPI_Win win);

  MPI_Info
  add_info_ptr(mpi_info* ptr);

  MPI_Win
  add_win_ptr(mpi_window* ptr, MPI_Win win);

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
  erase_err_handler(MPI_Comm comm);

  void
  add_err_handler(MPI_Comm comm, MPI_Errhandler err);

  MPI_Errhandler
  get_err_handler(MPI_Comm comm);

  void
  copy_err_handler(MPI_Comm dst, MPI_Comm src);

  void*
  find_buffer(MPI_Request req);

  void
  add_buffer(MPI_Request req, void* buffer);

  void
  add_keyval(int key, keyval* keyval);

  keyval*
  get_keyval(int key);

  void
  attach_buffer(void* buffer, int size);

  void
  detach_buffer(void** buffer, int* size);

  void
  erase_buffer(MPI_Request req);

 protected:
  /// Ensure that MPI is properly initialized.
  inline void
  assert_initialized() const;

  /// Nonblocking send -- actual implementation.
  /// wait for this request has the same completion as send.
  /// \return time when the request has been registered (likely current time).
  timestamp
  do_isend(int count, mpi_type_id type, mpi_id target,
           mpi_tag tag, mpi_comm* comm, mpi_request* req,
           const payload::const_ptr& content);

  /// Nonblocking send -- actual implementation.
  /// wait for this request has the same completion as send.
  /// \return time when the request has been registered (likely current time).
  timestamp
  do_ibsend(int count, mpi_type_id type, mpi_id target,
            mpi_tag tag, mpi_comm* comm, mpi_request* req,
            const payload::const_ptr& content);

  /// Nonblocking send -- actual implementation.
  /// wait for this request has the same completion as send.
  /// \return time when the request has been registered (likely current time).
  timestamp
  do_issend(int count, mpi_type_id type, mpi_id target,
            mpi_tag tag, mpi_comm* comm, mpi_request* req,
            const payload::const_ptr& content);

  /// Nonblocking send -- actual implementation.
  /// wait for this request has the same completion as send.
  /// \return time when the request has been registered (likely current time).
  timestamp
  do_irsend(int count, mpi_type_id type, mpi_id target,
            mpi_tag tag, mpi_comm* comm, mpi_request* req,
            const payload::const_ptr& content);

  /// Asynchronous receive -- actual implementation.
  /// wait for this request has the same completion as recv.
  /// \return time at which the receive request has been registered.
  timestamp
  do_irecv(int count, mpi_type_id type, mpi_id source,
           mpi_tag tag, mpi_comm* comm, mpi_request* req);


  timestamp
  do_wait(mpi_request* req);

  timestamp
  do_waitall(std::vector<mpi_request*>& req);

  timestamp
  do_waitany(std::vector<mpi_request*>& reqs, int& index,
    timestamp timeout);

  timestamp
  do_waitsome(std::vector<mpi_request*>& req,
    std::vector<int>& indices);

  timestamp
  do_test(mpi_request* req, bool& flag);

  timestamp
  do_testall(std::vector<mpi_request*>& req, bool& flag);

  timestamp
  do_testany(std::vector<mpi_request*>& req, int& index, bool& flag);

  timestamp
  do_testsome(std::vector<mpi_request*>& req,
          std::vector<int>& indices);

  void
  free_request(mpi_request** req);

  void
  free_request(std::vector<mpi_request*>& reqs, int index);

  void
  free_requests(std::vector<mpi_request*>& reqs);

  void
  free_requests(std::vector<mpi_request*>& reqs,
    const std::vector<int>& inds);

  void
  build_statuses(std::vector<mpi_request*>& reqs,
    std::vector<mpi_status>& stats);

  void
  build_statuses(std::vector<mpi_request*>& reqs,
    const std::vector<int>& inds,
    std::vector<mpi_status>& stats);

 protected:
  void
  precommit_types();

  void
  precommit_type(mpi_type* type, int id);

  std::string
  type_label(mpi_type_id tid);

 protected:
  software_id id_;

  payload::const_ptr saved_payload_;

  typedef spkt_unordered_map<mpi_request*, mpi_rma_message::op_info> rma_req_map;
  typedef spkt_unordered_map<mpi_window*, rma_req_map> pending_rma_map;
  pending_rma_map pending_rmas_;

 private:
  /// The MPI server.
  mpi_queue* queue_;

  //// My MPI index in the world.
  mpi_id rank_;

  mpi_type_id next_type_id_;

  /// The strategy object that gets consulted for how to do anything.
  mpi_strategy* strategy_;

  /// The builder for mpi communicators.
  mpi_comm_factory* comm_factory_;

  /// Ensure that collective operations are given unique tags
  /// to protect shared back-end data (magic payload info).

  /// The state of this object (initialized or not).
  enum {
    is_fresh, is_initialized, is_finalizing, is_finalized
  } status_;

  bool skip_comm_world_;

  /// MPI_COMM_WORLD.
  mpi_comm* worldcomm_;

  /// MPI_COMM_SELF.
  mpi_comm* selfcomm_;

  std::vector<mpi_request*> pending_bsends_;


  //----------------------------------------------------------------
  // --- MPI Derived Datatype
  // --- Presently, payloads won't work with derived datatypes
  //----------------------------------------------------------------
  typedef spkt_unordered_map<mpi_type_id, mpi_type*> type_map;
  type_map known_types_;

  spkt_unordered_map<long, mpi_op*> created_op_;

  typedef spkt_unordered_map<MPI_Comm, mpi_comm*> comm_ptr_map;
  comm_ptr_map comm_map_;
  typedef spkt_unordered_map<MPI_Group, mpi_group*> group_ptr_map;
  group_ptr_map grp_map_;
  MPI_Group group_counter_;


  typedef spkt_unordered_map<MPI_Comm, MPI_Group> comm_grp_map;
  comm_grp_map comm_grp_map_;
  typedef spkt_unordered_map<MPI_Info, mpi_info*> info_map;
  info_map info_map_;
  MPI_Info info_counter_;

  typedef spkt_unordered_map<MPI_Win, mpi_window*> win_map;
  win_map win_map_;

  std::pair<void*, int> attached_buffer_;

  struct win_rma_info {
    void* buffer;
    int count;
    MPI_Datatype datatype;
    mpi_window* win;
    enum rmatype {
      get, put, acc
    };
    rmatype type;
    mpi_op op;
  };

  spkt_unordered_map<mpi_request*, win_rma_info> pending_rmas;
  typedef spkt_unordered_map<MPI_Request, mpi_request*> req_ptr_map;
  req_ptr_map req_map_;
  MPI_Request req_counter_;

  typedef spkt_unordered_map<MPI_Comm, MPI_Errhandler> err_handler_map;
  err_handler_map err_handlers_;

  typedef spkt_unordered_map<MPI_Win, MPI_Errhandler> win_err_handler_map;
  win_err_handler_map win_err_handlers_;

  spkt_unordered_map<int, keyval*> keyvals_;

  typedef spkt_unordered_map<MPI_Request, void*> buf_map_t;
  buf_map_t buffers_;

};

mpi_api*
sstmac_mpi();

}
} // end of namespace sstmac

#endif

