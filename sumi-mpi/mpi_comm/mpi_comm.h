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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMM_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMM_H_INCLUDED

#include <sumi/communicator.h>
#include <sstmac/common/node_address.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/launch/app_launch.h>
#include <sumi-mpi/mpi_comm/keyval_fwd.h>
#include <sumi-mpi/mpi_comm/mpi_group.h>
#include <sumi-mpi/sstmac_mpi_integers.h>
#include <sumi-mpi/mpi_request_fwd.h>

namespace sumi {

using sstmac::sw::app_id;
using sstmac::sw::app_launch;
using sstmac::node_id;

/**
 * An MPI communicator handle.
 */
class mpi_comm : public communicator
{
 public:

  enum topotypes {
    TOPO_NONE, TOPO_GRAPH, TOPO_CART
  };

  static const int proc_null = -1;

 public:
  mpi_comm();

  /// Hello.
  mpi_comm(
    MPI_Comm id,
    int rank,
    mpi_group* peers,
    app_id aid,
    bool del_grp = false);

  /// Goodbye.
  virtual
  ~mpi_comm();

  void
  set_name(std::string name) {
    name_ = name;
  }

  std::string
  name() const {
    return name_;
  }

  static void
  delete_statics();

  topotypes
  topo_type() const {
    return topotype_;
  }

  mpi_group*
  group() {
    return group_;
  }

  void
  dup_keyvals(mpi_comm* m);

  /// This is the null communicator.
  static mpi_comm* comm_null;

  MPI_Comm id_;
  int rank_;

  virtual std::string
  to_string() const;

  /// The rank of this peer in the communicator.
  int
  rank() const {
    return rank_;
  }

  /// The size of the communicator.
  int
  size() const;

  /// The identifier for this communicator.
  /// To be used to tag messages to/from this communicator.
  MPI_Comm
  id() const {
    return id_;
  }

  void
  set_keyval(keyval* k, void* val);

  void
  get_keyval(keyval* k, void* val, int* flag);

  app_id
  app() const {
    return aid_;
  }

  int
  comm_to_global_rank(int comm_rank) const {
    return int(peer_task(comm_rank));
  }

  int
  global_to_comm_rank(int global_rank) const;

  int
  nproc() const {
    return size();
  }

  //
  // Get a unique tag for a collective operation.
  //
  int
  next_collective_tag();

  /// The task index of the caller.
  task_id
  my_task() const;

  /// The task index of the given peer.
  task_id
  peer_task(int rank) const;

  /// Equality comparison.
  inline bool
  operator==(mpi_comm* other) const {
    return ((rank_ == other->rank_) && (id_ == other->id_));
  }

  /// Inequality comparison.
  inline bool
  operator!=(mpi_comm* other) const {
    return !this->operator==(other);
  }

  void
  add_request(int tag, mpi_request* req){
    ireqs_[tag] = req;
  }

  mpi_request*
  get_request(int tag) const {
    std::map<int, mpi_request*>::const_iterator it = ireqs_.find(tag);
    if (it == ireqs_.end()){
      spkt_throw_printf(sprockit::value_error,
          "cannot find tag %d on comm %d for returning collective MPI_Request",
          tag, id_);
    }
    return it->second;
  }

 protected:
  friend std::ostream&
  operator<<(std::ostream &os, mpi_comm* comm);

  void
  validate(const char* fxn) const;

 protected:
  friend class mpi_comm_factory;

  /// The tasks participating in this communicator.  This is only used for an mpicomm* which is NOT WORLD_COMM.
  mpi_group* group_;

  uint16_t next_collective_tag_;

  spkt_unordered_map<int, keyval*> keyvals_;

  app_id aid_;

  bool del_grp_;

  topotypes topotype_;

  std::string name_;

  std::map<int, mpi_request*> ireqs_;


};

/// Fairly self-explanatory.
std::ostream&
operator<<(std::ostream &os, mpi_comm* comm);

}

#endif

