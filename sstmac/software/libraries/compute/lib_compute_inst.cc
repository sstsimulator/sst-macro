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

#include <sstmac/common/event_callback.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>

RegisterDebugSlot(lib_compute_inst);

namespace sstmac {
namespace sw {

static const char* deprecated[] = { "lib_compute_unroll_loops" };
static sprockit::StaticKeywordRegister deprecated_keys(1, deprecated);

lib_compute_inst::lib_compute_inst(const std::string& libname, software_id id)
  : lib_compute(libname, id)
{
}

lib_compute_inst::lib_compute_inst(software_id sid) :
  lib_compute("computelibinstr%s", sid)
{
}

void
lib_compute_inst::compute_detailed(
  uint64_t flops,
  uint64_t nintops,
  uint64_t bytes)
{
  /** Configure the compute request */
  auto cmsg = new compute_event_impl<basic_instructions_st>;
  basic_instructions_st& st = cmsg->data();
  st.flops = flops;
  st.intops = nintops;
  st.mem_sequential = bytes;
  compute_inst(cmsg);
  delete cmsg;
}

void
lib_compute_inst::compute_loop(uint64_t num_loops,
  uint32_t flops_per_loop,
  uint32_t nintops_per_loop,
  uint32_t bytes_per_loop)
{
  /** Configure the compute request */
  uint64_t loop_control_ops = 2 * num_loops * loop_overhead_;
  uint64_t num_intops = loop_control_ops + nintops_per_loop*num_loops;
  compute_detailed(
    flops_per_loop*num_loops,
    num_intops,
    bytes_per_loop*num_loops);
}

void
lib_compute_inst::consume_params(sprockit::sim_parameters* params)
{
  if (params->has_param("lib_compute_unroll_loops")){
    double loop_unroll = params->deprecated_double_param("lib_compute_unroll_loops");
    loop_overhead_ = 1.0 / loop_unroll;
  }
  else {
    loop_overhead_ = params->get_optional_double_param("lib_compute_loop_overhead", 1.0);
  }
}

void
lib_compute_inst::compute_inst(compute_event* cmsg)
{
  SSTMACBacktrace("Compute Instructions");
  os_->execute(ami::COMP_INSTR, cmsg, key_category);
}

}
} //end of namespace sstmac

