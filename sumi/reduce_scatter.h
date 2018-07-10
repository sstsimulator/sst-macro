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

#ifndef sstmac_sw_api_simpsg_REDUCE_SCATTER_H
#define sstmac_sw_api_simpsg_REDUCE_SCATTER_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class halving_reduce_scatter_actor :
  public dag_collective_actor
{

 public:
  std::string to_string() const override {
    return "virtual all reduce actor";
  }

  void buffer_action(void *dst_buffer,
                void *msg_buffer, action* ac) override;

  halving_reduce_scatter_actor(reduce_fxn fxn) : fxn_(fxn) {}

 private:
  bool is_lower_partner(int virtual_me, int partner_gap);
  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;

 private:
  reduce_fxn fxn_;

  int num_reducing_rounds_;

  int num_total_rounds_;

};

class halving_reduce_scatter :
  public dag_collective
{
  FactoryRegister("halving-rs", dag_collective, halving_reduce_scatter)

 public:
  std::string to_string() const override {
    return "sumi allreduce";
  }

  halving_reduce_scatter(reduce_fxn fxn) : fxn_(fxn) {}

  halving_reduce_scatter(){}

  virtual void init_reduce(reduce_fxn fxn) override {
    fxn_ = fxn;
  }

  dag_collective_actor* new_actor() const override {
    return new halving_reduce_scatter_actor(fxn_);
  }

  dag_collective* clone() const override {
    return new halving_reduce_scatter(fxn_);
  }

 private:
  reduce_fxn fxn_;

};

}

#endif
