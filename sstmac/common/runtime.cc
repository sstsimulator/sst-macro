/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sprockit/statics.h>
#include <sstmac/common/runtime.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/launch/task_mapping.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {

bool Runtime::do_deadlock_check_ = false;
std::list<deadlock_check*> Runtime::deadlock_checks_;
sw::JobLauncher* Runtime::launcher_ = nullptr;
hw::Topology* Runtime::topology_ = nullptr;

void
Runtime::checkDeadlock()
{
  if (!do_deadlock_check_) return;

  std::list<deadlock_check*>::iterator it, end = deadlock_checks_.end();
  for (it=deadlock_checks_.begin(); it != end; ++it){
    deadlock_check* check = *it;
    check->run();
  }
}

void
Runtime::clearStatics()
{
  //hw::interconnect::clear_staticInterconnect();
  //parallel_runtime::clearStaticRuntime();
  hw::Topology::clearStaticTopology();
}

NodeId
Runtime::current_node()
{
  return sw::OperatingSystem::currentNodeId();
}

void
Runtime::deleteStatics()
{
  //not owned by runtime - do not delete
  //if (topology_) delete topology_;
  //if (launcher_) delete launcher_;
  for (deadlock_check* chk: deadlock_checks_){
    delete chk;
  }
  deadlock_checks_.clear();
  clearStatics();
}

NodeId
Runtime::nodeForTask(sw::AppId aid, sw::TaskId tid)
{
  return sstmac::sw::TaskMapping::globalMapping(aid)->rankToNode(tid);
}

void
Runtime::finish()
{
}

}
