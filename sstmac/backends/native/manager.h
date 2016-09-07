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
class manager : public sprockit::factory_type {

 public:
  manager();

  virtual std::string
  to_string() const {
    return "manager";
  }

  /// Goodbye.
  virtual ~manager() throw ();

  void
  init_factory_params(sprockit::sim_parameters* params);

  virtual timestamp
  run(timestamp until) = 0;

  virtual void stop() = 0;

  virtual void finish() = 0;

  sstmac::hw::interconnect*
  interconn() const {
    return interconnect_;
  }

  static int
  compute_max_nproc(sprockit::sim_parameters *params);

  static int
  compute_max_nproc_for_app(sprockit::sim_parameters* app_params);

 protected:
  /// Next parallel process id.
  sstmac::sw::app_id next_ppid_;

  sstmac::hw::interconnect* interconnect_;
  parallel_runtime* rt_;

};

#if SSTMAC_INTEGRATED_SST_CORE
class sst_manager : public manager
{
  virtual void
  init_factory_params(sprockit::sim_parameters *params);
};
#else
class macro_manager : public manager
{
 public:
  macro_manager(parallel_runtime* rt);

  virtual ~macro_manager() throw();

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  /**
   * @brief run
   * @param until The time to run until. Negative value indicates run until no more events
   * @return The time of the last event
   */
  timestamp run(timestamp until = timestamp(-1));

  void finish();

 private:
  void start();

  void stop();

 private:
  /// The event manager.
  event_manager* event_manager_;

  /// Monitor whether the simulator is currently running.
  bool running_;


};
#endif

}
} // end of namespace sstmac.

#endif

