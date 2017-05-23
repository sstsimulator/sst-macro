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

#ifndef sumi_api_COLLECTIVE_H
#define sumi_api_COLLECTIVE_H

#include <sumi/timeout.h>
#include <sumi/collective_message_fwd.h>
#include <sumi/transport_fwd.h>
#include <sumi/communicator_fwd.h>
#include <sumi/collective_actor_fwd.h>
#include <sumi/comm_functions.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(sumi_collective)
DeclareDebugSlot(sumi_vote)
DeclareDebugSlot(sumi_collective_sendrecv)
DeclareDebugSlot(sumi_collective_init)
DeclareDebugSlot(sumi_collective_round)

namespace sumi {

class collective
{
 public:
  typedef enum {
    alltoall,
    alltoallv,
    allreduce,
    allgather,
    allgatherv,
    bcast,
    barrier,
    gather,
    gatherv,
    reduce,
    reduce_scatter,
    scan,
    scatter,
    scatterv,
    dynamic_tree_vote,
    heartbeat
  } type_t;

  virtual std::string
  to_string() const = 0;

  virtual ~collective();

  /**
   * @brief persistent
   * Some collectives are not allowed to "exit" based on the protocol
   * They have to remain active and persistent even after receving a completion ack
   * @return Whether the collective is persistent
   */
  virtual bool
  persistent() const {
    return false;
  }

  int
  context() const {
    return context_;
  }

  static const char*
  tostr(type_t ty);

  virtual void
  recv(int target, const collective_work_message_ptr& msg) = 0;

  void
  recv(const collective_work_message_ptr &msg);

  virtual void
  start() = 0;

  communicator*
  comm() const {
    return comm_;
  }

  bool
  complete() const {
    return complete_;
  }

  void
  set_complete() {
    complete_ = true;
  }

  int
  tag() const {
    return tag_;
  }

  type_t
  type() const {
    return type_;
  }

  void
  actor_done(int comm_rank, bool& generate_cq_msg, bool& delete_event);

  virtual void
  add_actors(collective* coll);

  static const int default_nproc = -1;

  virtual void
  deadlock_check(){}

  void
  init(type_t type, transport* api, communicator* comm, int tag, int context);

  virtual void init_actors(){}

 protected:
  collective(type_t type, transport* api, communicator* comm, int tag, int context);

  collective(){} //to be initialized later

 protected:
  transport* my_api_;
  communicator* comm_;
  int dense_me_;
  int dense_nproc_;
  int context_;
  bool complete_;
  int tag_;

  std::map<int, int> refcounts_;
  collective::type_t type_;

};

class dag_collective :
  public collective
{
  DeclareFactory(dag_collective)

 public:
  void
  recv(int target, const collective_work_message_ptr& msg);

  void
  start();

  void
  init(type_t type,
    transport *my_api, communicator *comm,
    void *dst, void *src,
    int nelems, int type_size,
    int tag,
    bool fault_aware, int context);

  void init_actors();

  virtual dag_collective*
  clone() const = 0;

  virtual void
  init_reduce(reduce_fxn fxn){}

  virtual void
  init_root(int root){}

  virtual void init_recv_counts(int* nelems){}

  virtual void init_send_counts(int* nelems){}

  void deadlock_check();

  virtual ~dag_collective();

  static dag_collective*
  construct(const std::string& name, sprockit::sim_parameters* params, reduce_fxn fxn);

  static dag_collective*
  construct(const std::string& name, sprockit::sim_parameters *params);

 protected:
  virtual dag_collective_actor*
  new_actor() const = 0;

  void add_actors(collective *coll);

 protected:
  typedef std::map<int, dag_collective_actor*> actor_map;
  actor_map my_actors_;

  void* src_buffer_;

  void* dst_buffer_;

  int nelems_;

  int type_size_;

  // The bruck algorithm can actually apply to multiple collectives
  // Mainly barrier and allgather


  bool fault_aware_;

  std::list<collective_work_message_ptr> pending_;
};

class collective_algorithm_selector
{
 public:
  virtual dag_collective* select(int nproc, int nelems) = 0;
  virtual dag_collective* select(int nproc, int* counts) = 0;
};


}

#endif // COLLECTIVE_H