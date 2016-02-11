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

#ifndef SSTMAC_HARDWARE_PROCESSOR_COMPUTESCHEDULER_H_INCLUDED
#define SSTMAC_HARDWARE_PROCESSOR_COMPUTESCHEDULER_H_INCLUDED

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(compute_scheduler)

namespace sstmac {
namespace sw {

class compute_scheduler :
  public sprockit::factory_type
{
 public:

 public:
  virtual std::string
  to_string() const {
    return "compute_scheduler";
  }

  virtual ~compute_scheduler() {}

  void
  init_factory_params(sprockit::sim_parameters* params);
  
  virtual void
  init_param1(sw::operating_system* os) {
    os_ = os;
  }

  int
  ncores() const {
    return ncores_;
  }

  int
  nsocket() const {
    return nsocket_;
  }

  virtual void
  finalize_init();

  virtual compute_scheduler*
  clone(sw::operating_system* os) const = 0;

  virtual void
  reserve_core(thread* thr) = 0;
  
  virtual void
  release_core(thread* thr) = 0;
  
  virtual void
  configure(int ncore, int nsocket);

 protected:
  compute_scheduler();

  void
  clone_into(compute_scheduler* cln) const;

 protected:
  int ncores_;
  int nsocket_;
  int cores_per_socket_;
  sw::operating_system* os_;

};

DeclareFactory1InitParam(compute_scheduler, sw::operating_system*);

}
} //end of namespace sstmac
#endif

