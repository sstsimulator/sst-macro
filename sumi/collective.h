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

#ifndef sumi_api_COLLECTIVE_H
#define sumi_api_COLLECTIVE_H

#include <sumi/timeout.h>
#include <sumi/collective_message_fwd.h>
#include <sumi/transport_fwd.h>
#include <sumi/communicator_fwd.h>
#include <sumi/collective_actor_fwd.h>
#include <sumi/comm_functions.h>
#include <sumi/options.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(sumi_collective)
DeclareDebugSlot(sumi_vote)
DeclareDebugSlot(sumi_collective_init)

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
    scatterv
  } type_t;

  virtual std::string toString() const = 0;

  virtual ~collective();

  /**
   * @brief persistent
   * Some collectives are not allowed to "exit" based on the protocol
   * They have to remain active and persistent even after receving a completion ack
   * @return Whether the collective is persistent
   */
  virtual bool persistent() const {
    return false;
  }

  static const char* tostr(type_t ty);

  virtual CollectiveDoneMessage* recv(int target, collective_work_message* msg) = 0;

  CollectiveDoneMessage* recv(collective_work_message* msg);

  virtual void start() = 0;

  Communicator* comm() const {
    return comm_;
  }

  bool complete() const {
    return complete_;
  }

  void set_complete() {
    complete_ = true;
  }

  int tag() const {
    return tag_;
  }

  type_t type() const {
    return type_;
  }

  void actor_done(int comm_rank, bool& generate_cq_msg, bool& delete_event);

  virtual CollectiveDoneMessage* add_actors(collective* coll);

  static const int default_nproc = -1;

  virtual void deadlock_check(){}

  virtual void init_actors(){}

 protected:
  collective(type_t type, CollectiveEngine* engine, int tag, int cq_id, Communicator* comm);

 protected:
  Transport* my_api_;
  CollectiveEngine* engine_;
  int cq_id_;
  Communicator* comm_;
  int dom_me_;
  int dom_nproc_;
  bool complete_;
  int tag_;

  std::map<int, int> refcounts_;
  collective::type_t type_;

};

class DagCollective :
  public collective
{
 public:
  CollectiveDoneMessage* recv(int target, collective_work_message* msg) override;

  void start() override;

  void deadlock_check() override;

  void init_actors() override;

  virtual ~DagCollective();

 protected:
  virtual DagCollectiveActor* newActor() const = 0;

  CollectiveDoneMessage* add_actors(collective *coll) override;

 protected:
  DagCollective(collective::type_t ty, CollectiveEngine* engine, void *dst, void *src,
                 int type_size, int tag, int cq_id, Communicator* comm) :
    collective(ty, engine, tag, cq_id, comm),
    src_buffer_(src), dst_buffer_(dst), type_size_(type_size), fault_aware_(false)
  {
  }

  typedef std::map<int, DagCollectiveActor*> actor_map;
  actor_map my_actors_;

  void* src_buffer_;

  void* dst_buffer_;

  int type_size_;

  bool fault_aware_;

  std::list<collective_work_message*> pending_;
};

}

#endif // COLLECTIVE_H
