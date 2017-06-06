/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#include <sstmac/common/runtime.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {

bool runtime::do_deadlock_check_ = false;
std::list<deadlock_check*> runtime::deadlock_checks_;
sw::job_launcher* runtime::launcher_ = nullptr;
hw::topology* runtime::topology_ = nullptr;

void
runtime::check_deadlock()
{
  if (!do_deadlock_check_) return;

  std::list<deadlock_check*>::iterator it, end = deadlock_checks_.end();
  for (it=deadlock_checks_.begin(); it != end; ++it){
    deadlock_check* check = *it;
    check->run();
  }
}

void
runtime::clear_statics()
{
  hw::interconnect::clear_static_interconnect();
  //parallel_runtime::clear_static_runtime();
  hw::topology::clear_static_topology();
}

node_id
runtime::current_node()
{
  return sw::operating_system::current_node_id();
}

void
runtime::delete_statics()
{
  //not owned by runtime - do not delete
  //if (topology_) delete topology_;
  //if (launcher_) delete launcher_;
  for (deadlock_check* chk: deadlock_checks_){
    delete chk;
  }
  deadlock_checks_.clear();
  clear_statics();
}

node_id
runtime::node_for_task(sw::app_id aid, sw::task_id tid)
{
  return sstmac::sw::task_mapping::global_mapping(aid)->rank_to_node(tid);
}

void
runtime::finish()
{
}

}