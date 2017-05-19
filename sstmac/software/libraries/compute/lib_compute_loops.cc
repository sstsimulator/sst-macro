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

#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/libraries/compute/lib_compute_loops.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/common/sstmac_env.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <stdint.h>
#include <math.h>

RegisterKeywords(
"lib_compute_loop_overhead",
"lib_compute_loops_enable",
"lib_compute_loops_mem_ratio",
"lib_compute_loops_flop_ratio",
"lib_compute_access_width",
);

namespace sstmac {
namespace sw {

int lib_compute_loops::mem_op_size_ = 16;

double lib_compute_loops::mem_line_ratio_ = -1;
double lib_compute_loops::flop_line_ratio_ = -1;
bool lib_compute_loops::do_loops_ = true;

lib_compute_loops::lib_compute_loops(sprockit::sim_parameters* params, software_id id,
                                     operating_system* os) :
  lib_compute_memmove(params, "lib_compute_loops", id, os)
{
  key_cat_ = lib_compute::key_category;
  mem_line_ratio_ = params->get_optional_double_param(
                        "lib_compute_loops_mem_ratio", 0.8);
  flop_line_ratio_ = params->get_optional_double_param(
                         "lib_compute_loops_flop_ratio", 0.8);
  do_loops_ = params->get_optional_bool_param(
                  "lib_compute_loops_enable", true);
}

void
lib_compute_loops::compute_loop_work(long long loop, double numlines)
{
  SSTMACBacktrace("Compute Loops");
  if (loop < 0) {
    loop = 0;
  }

  if (numlines < 0) {
    numlines = 0;
  }


  doing_memory_ = true;
  long long bytes = std::max((long long) 1,
                             (long long) (loop * mem_line_ratio_ * numlines * mem_op_size_));
  lib_compute_memmove::read(bytes);
  doing_memory_ = false;

  basic_compute_event* inst = new basic_compute_event;
  basic_instructions_st& st = inst->data();
  st.flops = std::max((long long) 1,
                       (long long) (loop * flop_line_ratio_ * numlines));
  lib_compute_inst::compute_inst(inst);
  delete inst;
}



void
lib_compute_loops::compute_fft()
{
  if (do_loops_) {
    basic_compute_event* inst = new basic_compute_event;
    basic_instructions_st& st = inst->data();
    st.flops = 500;
    lib_compute_inst::compute_inst(inst);
    delete inst;
  }
}

}
} //end of namespace sstmac