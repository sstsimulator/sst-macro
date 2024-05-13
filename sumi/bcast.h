/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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

#ifndef sumi_bcast_included_h
#define sumi_bcast_included_h

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class BinaryTreeBcastActor :
  public DagCollectiveActor
{
 public:
  BinaryTreeBcastActor(CollectiveEngine* engine, int root, void *buf, int nelems,
                          int type_size, int tag, int cq_id, Communicator* comm)
    : DagCollectiveActor(Collective::bcast, engine, buf, buf, type_size, tag, cq_id, comm),
      root_(root), nelems_(nelems)
  {}

  std::string toString() const override {
    return "bcast actor";
  }

  ~BinaryTreeBcastActor(){}

 private:
  void finalizeBuffers() override;
  void initBuffers() override;
  void initDag() override;

  void init_root(int me, int roundNproc, int nproc);
  void init_child(int me, int roundNproc, int nproc);
  void init_internal(int me, int windowSize, int windowStop, Action* recv) ;
  void bufferAction(void *dst_buffer, void *msg_buffer, Action *ac) override;

  int root_;
  int nelems_;
};

class BinaryTreeBcastCollective :
  public DagCollective
{
 public:
  BinaryTreeBcastCollective(CollectiveEngine* engine, int root, void* buf,
                               int nelems, int type_size, int tag, int cq_id, Communicator* comm)
    : DagCollective(Collective::bcast, engine, buf, buf, type_size, tag, cq_id, comm),
      root_(root), nelems_(nelems) {}

  std::string toString() const override {
    return "bcast";
  }

  DagCollectiveActor* newActor() const override {
    return new BinaryTreeBcastActor(engine_, root_, dst_buffer_, nelems_,
                                       type_size_, tag_, cq_id_, comm_);
  }

 private:
  int root_;
  int nelems_;

};

}

#endif // BCAST_H
