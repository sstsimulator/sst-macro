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

