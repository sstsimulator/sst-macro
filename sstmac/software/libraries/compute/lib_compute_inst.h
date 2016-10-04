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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_INST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_INST_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/common/sstmac_config.h>
#include <stdint.h>
//these are the default instruction labels

DeclareDebugSlot(lib_compute_inst);

namespace sstmac {
namespace sw {

class lib_compute_inst :
  public lib_compute
{
 public:
  lib_compute_inst(sprockit::sim_parameters* params, software_id id, operating_system* os);

  lib_compute_inst(sprockit::sim_parameters* params, const std::string& libname,
                   software_id id, operating_system* os);

  virtual
  ~lib_compute_inst() { }

  void
  compute_inst(compute_event* msg);

  void
  compute_detailed(uint64_t flops,
    uint64_t nintops,
    uint64_t bytes);

  void
  compute_loop(uint64_t nloops,
    uint32_t flops_per_loop,
    uint32_t intops_per_loop,
    uint32_t bytes_per_loop);

  virtual void
  incoming_event(event *ev) override {
    library::incoming_event(ev);
  }

 protected:
  double loop_overhead_;

 private:
  void init(sprockit::sim_parameters* params);

};

}
} //end of namespace sstmac

#endif

