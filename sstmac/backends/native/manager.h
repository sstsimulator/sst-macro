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

#ifndef SSTMAC_BACKENDS_NATIVE_MANAGER_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_MANAGER_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/launch/app_launch_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>
#include <sprockit/factories/factory.h>

DeclareDebugSlot(timestamp);

namespace sstmac {
namespace native {

/**
 * The owner of a discrete-event network simulation.
 *
 * All time progression is handled by this object
 * and messages between nodes are managed here as well.
 */
class manager {

 public:
  manager(sprockit::sim_parameters* params, parallel_runtime* rt);

  static int
  compute_max_nproc(sprockit::sim_parameters *params);

  static int
  compute_max_nproc_for_app(sprockit::sim_parameters* app_params);

#if !SSTMAC_INTEGRATED_SST_CORE
  ~manager() throw ();

  timestamp
  run(timestamp until);

  void stop();

  void finish();

  sstmac::hw::interconnect*
  interconn() const {
    return interconnect_;
  }

 private:
  void start();

  event_manager* event_manager_;

  bool running_;

  sstmac::sw::app_id next_ppid_;

  sstmac::hw::interconnect* interconnect_;
  parallel_runtime* rt_;
#endif
};

}
} // end of namespace sstmac.

#endif

