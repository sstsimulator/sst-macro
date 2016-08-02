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

namespace sstmac {
namespace hw {

SpktRegister("instruction", processor, instruction_processor,
            "Extension of simpleprocessor that estimates compute time of instruction counters");

instruction_processor::~instruction_processor()
{
  if (noise_model_) delete noise_model_;
}

void
instruction_processor::finalize_init()
{
  simple_processor::finalize_init();

  tflop_ = tintop_ = 1.0 / freq_;
  tmemseq_ = tmemrnd_ = 1.0 / mem_freq_;
  max_single_mem_bw_ = mem_->max_single_bw();
}

void
instruction_processor::init_factory_params(sprockit::sim_parameters* params)
{
  simple_processor::init_factory_params(params);
  timestamp negligible_time = params->get_optional_time_param("negligible_compute_time", 100e-9);
  negligible_time_sec_ = negligible_time.sec();
  negligible_bytes_ = params->get_optional_byte_length_param("negligible_compute_bytes", 64);

  parallelism_ = params->get_optional_double_param("parallelism", 1.0);

  if (params->has_namespace("noise")){
    sprockit::sim_parameters* noise_params = params->get_namespace("noise");
    noise_model_ = noise_model_factory::get_param("model", noise_params);
  }

}

double
instruction_processor::instruction_time(sw::compute_event* cmsg)
{
  double tsec = 0;
  long nop = 0;
  nop = cmsg->event_value(sw::compute_event::flop);
  double tintop, tflop;
  if (noise_model_){
    tflop = tintop = 1.0/noise_model_->value();
  } else {
    tintop = tintop_;
    tflop = tflop_;
  }
  tsec += nop*tflop/parallelism_;
  nop = cmsg->event_value(sw::compute_event::intop);
  tsec += nop*tintop/parallelism_;
  if (tsec < 0){
    spkt_throw_printf(sprockit::value_error,
        "instruction_processor: computed negative instruction time of %8.4e sec",
        tsec);
  }
  return tsec;
}

void
instruction_processor::do_compute(sw::compute_event* ev)
{
  debug_printf(sprockit::dbg::compute_intensity,
    "Node %d: starting compute %s",
    int(node_->addr()),
    ev->debug_string().c_str());

  if (ev->timed_computed()){
    node_->compute(ev->event_time());
  }
  else {
    // compute execution time in seconds
    double instr_time = instruction_time(ev);
    // now count the number of bytes
    long bytes = ev->event_value(sw::compute_event::mem_sequential);
    // max_single_mem_bw is the bandwidth achievable if ZERO instructions are executed
    double best_possible_time = instr_time + bytes / max_single_mem_bw_;
    if (best_possible_time < negligible_time_sec_){
      //no delay, just continue on
    }
    else if (bytes <= negligible_bytes_) {
      node_->compute(timestamp(instr_time));
    }
    else {
      //do the full memory modeling
      double best_possible_bw = bytes / best_possible_time;
      mem_->access(bytes, best_possible_bw);
    }
  }

}



}
} // end of namespace sstmac


