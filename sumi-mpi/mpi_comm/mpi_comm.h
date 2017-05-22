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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMM_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMM_H_INCLUDED

#include <sumi/communicator.h>
#include <sstmac/common/node_address.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sumi-mpi/mpi_comm/keyval_fwd.h>
#include <sumi-mpi/mpi_comm/mpi_group.h>
#include <sumi-mpi/mpi_integers.h>
#include <sumi-mpi/mpi_request_fwd.h>
#include <sprockit/unordered.h>
#include <sprockit/errors.h>
#include <string>
#include <map>

namespace sumi {

using sstmac::sw::app_id;
using sstmac::node_id;

/**
 * An MPI communicator handle.
 */
class mpi_comm : public communicator
{
 public:
  friend class mpi_api;

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
    bool del_grp = false,
    topotypes ty = TOPO_NONE);

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

  std::string
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
    auto it = ireqs_.find(tag);
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

 private:
  friend class mpi_comm_factory;

  void set_id(MPI_Comm id){
    id_ = id;
  }

  /// The tasks participating in this communicator.  This is only used for an mpicomm* which is NOT WORLD_COMM.
  mpi_group* group_;

  uint16_t next_collective_tag_;

  spkt_unordered_map<int, keyval*> keyvals_;

  app_id aid_;

  bool del_grp_;

  topotypes topotype_;

  std::string name_;

  std::map<int, mpi_request*> ireqs_;

  MPI_Comm id_;

 protected:
  int rank_;

};

/// Fairly self-explanatory.
std::ostream&
operator<<(std::ostream &os, mpi_comm* comm);

}

#endif