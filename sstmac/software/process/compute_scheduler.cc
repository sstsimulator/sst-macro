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

#include <sstmac/software/process/compute_scheduler.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::sw::compute_scheduler);
RegisterDebugSlot(compute_scheduler, "Print all debug information related to the OS compute scheduler");

namespace sstmac {
namespace sw {

compute_scheduler::compute_scheduler()
{
}

void
compute_scheduler::configure(int ncores, int nsocket)
{
  ncores_ = ncores * nsocket;
  nsocket_ = nsocket;
  cores_per_socket_ = ncores;
}

} //end namespace hw
} //end of namespace sstmac

