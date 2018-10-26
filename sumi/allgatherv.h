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

#ifndef sstmac_sw_api_simpsg_ALLGATHERV_H
#define sstmac_sw_api_simpsg_ALLGATHERV_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class bruck_allgatherv_actor :
  public bruck_actor
{

 public:
  bruck_allgatherv_actor(collective_engine* engine, void *dst, void *src, int* recv_counts,
                         int type_size, int tag, int cq_id, communicator* comm) :
    bruck_actor(collective::allgatherv, engine, dst, src, type_size, tag, cq_id, comm),
    recv_counts_(recv_counts)
  {
    total_nelems_ = 0;
    for (int i=0; i < dom_nproc_; ++i){
      if (i == dom_me_)
        my_offset_ = total_nelems_;
      total_nelems_ += recv_counts_[i];
    }
  }

  std::string to_string() const override {
    return "bruck allgatherv actor";
  }

 private:
  void finalize() override;

  void finalize_buffers() override;
  void init_buffers() override;
  void init_dag() override;

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

  int nelems_to_recv(int partner, int partner_gap);

  int* recv_counts_;
  int total_nelems_;
  int my_offset_;

};

class bruck_allgatherv_collective :
  public dag_collective
{
 public:
  bruck_allgatherv_collective(collective_engine* engine, void *dst, void *src, int* recv_counts,
                              int type_size, int tag, int cq_id, communicator* comm) :
    dag_collective(allgatherv, engine, dst, src, type_size, tag, cq_id, comm),
    recv_counts_(recv_counts)
  {
  }

  std::string to_string() const override {
    return "bruck allgatherv";
  }

  dag_collective_actor* new_actor() const override {
    return new bruck_allgatherv_actor(engine_, dst_buffer_, src_buffer_, recv_counts_, type_size_,
                                      tag_, cq_id_, comm_);
  }

 private:
  int* recv_counts_;

};

}

#endif // ALLGATHER_H
