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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>

RegisterDebugSlot(lib_compute_inst);

namespace sstmac {
namespace sw {

static const char* deprecated[] = { "lib_compute_unroll_loops" };
static sprockit::StaticKeywordRegister deprecated_keys(1, deprecated);

lib_compute_inst::lib_compute_inst(software_id id)
{
  libname_ = "computelibinstr" + id.to_string();
}

lib_compute_inst::lib_compute_inst(const std::string& id)
{
  libname_ = id;
}

bool
lib_compute_inst::supported() const
{
  return os_->kernel_supported(ami::COMP_INSTR);
}

void
lib_compute_inst::compute_detailed(
  uint64_t flops,
  uint64_t nintops,
  uint64_t bytes)
{
  /** Configure the compute request */
  compute_message::ptr cmsg = new compute_message();
  cmsg->set_event_value(compute_message::flop, flops);
  cmsg->set_event_value(compute_message::intop, nintops);
  cmsg->set_event_value(compute_message::mem_sequential, bytes);

  compute_inst(cmsg);
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
lib_compute_inst::compute_inst(const compute_message::ptr& cmsg)
{
  SSTMACBacktrace("Compute Instructions", cmsg->has_callback());

  os_->execute_kernel(ami::COMP_INSTR, cmsg);

  debug_printf(sprockit::dbg::compute_intensity,
    "Node %d: finishing compute %s",
    int(os_->my_addr()),
    cmsg->debug_string().c_str());
}

}
} //end of namespace sstmac

