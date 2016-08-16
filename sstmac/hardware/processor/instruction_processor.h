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

#ifndef SSTMAC_HARDWARE_PROCESSOR_INSTRUCTIONPROCESSOR_H_INCLUDED
#define SSTMAC_HARDWARE_PROCESSOR_INSTRUCTIONPROCESSOR_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/rng.h>
#include <sstmac/hardware/noise/noise.h>
#include <sstmac/hardware/processor/simple_processor.h>

namespace sstmac {
namespace hw {

class instruction_processor :
  public simple_processor
{
 public:
  instruction_processor(memory_model* mem, node* nd) :
    simple_processor(mem, nd),
    noise_model_(nullptr)
  {
  }

  virtual std::string
  to_string() const {
    return "instruction processor";
  }

  virtual ~instruction_processor();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  virtual void
  compute(event* ev, callback* cb);

 protected:
  void
  set_memop_distribution(double stdev);

  void
  set_flop_distribution(double stdev);

  double
  instruction_time(sw::basic_compute_event* msg);

 protected:
  double tflop_;
  double tintop_;
  double tmemseq_;
  double tmemrnd_;

  double max_single_mem_bw_;

  double negligible_bytes_;

  double parallelism_;

  noise_model* noise_model_;

};

}
} // end of namespace sstmac

#endif

