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

#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/libraries/compute/lib_compute.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/processor/instruction_processor.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>
#include <iostream>

RegisterNamespaces("noise");
RegisterKeywords(
{ "processor", "the model for the processor in the computation" },
{ "parallelism", "the degree of ILP in the processor" },
{ "pipeline_speedup", "the degree of ILP in the processor" },
{ "node_pipeline_speedup", "DEPRECATED: a speedup factor for computation "},
);

namespace sstmac {
namespace hw {

instruction_processor::~instruction_processor()
{
}

instruction_processor::
instruction_processor(sprockit::sim_parameters* params,
                      memory_model* mem, node* nd) :
  simple_processor(params, mem, nd)
{
  negligible_bytes_ = params->get_optional_byte_length_param(
        "negligible_compute_bytes", 64);

  double parallelism = params->get_optional_double_param("parallelism", 1.0);

  tflop_ = timestamp(1.0 / freq_ / parallelism);
  tintop_ = tflop_;
  tmemseq_ = timestamp(1.0 / mem_freq_);
  tmemrnd_ = tmemseq_;
  max_single_mem_inv_bw_ = timestamp(1.0 / mem_->max_single_bw());
}


timestamp
instruction_processor::instruction_time(sw::basic_compute_event* cmsg)
{
  sw::basic_instructions_st& st = cmsg->data();
  timestamp tsec = 0;
  tsec += st.flops*tflop_;
  tsec += st.intops*tintop_;
  return tsec;
}

void
instruction_processor::compute(event* ev, callback* cb)
{
  sw::basic_compute_event* bev = test_cast(sw::basic_compute_event, ev);
  sw::basic_instructions_st& st = bev->data();
  int nthread = st.nthread;
  // compute execution time in seconds
  timestamp instr_time = instruction_time(bev) / nthread;
  // now count the number of bytes
  uint64_t bytes = st.mem_sequential;
  // max_single_mem_bw is the bandwidth achievable if ZERO instructions are executed
  timestamp best_possible_time = instr_time + bytes * max_single_mem_inv_bw_;
  if (bytes <= negligible_bytes_) {
    node_->send_delayed_self_event_queue(instr_time, cb);
  } else {
    //do the full memory modeling
    double best_possible_bw = bytes / best_possible_time.sec();
    mem_->access(bytes, best_possible_bw, cb);
  }
}



}
} // end of namespace sstmac
