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

#include <sstmac/software/process/pmi.h>
#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/thread_lock.h>

namespace sstmac {
namespace sw {

process_manager::app_to_proc_to_node_map process_manager::node_map_;
process_manager::app_to_node_to_proc_map process_manager::proc_map_;

process_manager::process_manager(software_id sid, operating_system* os) :
  sid_(sid), my_os_(os)
{
  node_id addr = runtime::node_for_task(sid.app_, sid.task_);
  static thread_lock lock;
  lock.lock();
  node_map_[sid.app_][sid.task_] = addr;
  proc_map_[sid.app_][addr] = sid.task_;
  lock.unlock();
  my_addr_ = addr;
}

process_manager::~process_manager()
{
}

void
process_manager::kill_node()
{
#if !SSTMAC_INTEGRATED_SST_CORE
  my_os_->kill_node();
#else
  spkt_throw(sprockit::unimplemented_error,
    "process_manager::kill_node");
#endif
}

void
process_manager::kill_process()
{
  spkt_throw(sprockit::unimplemented_error,
    "process_manager::kill_process");
}

int
process_manager::get_partner(node_id addr) const
{
  auto it1 = proc_map_.find(sid_.app_);
  if (it1 == proc_map_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "process_manager::get_partner: "
                     "Could not find app %d. "
                     "Are you sure process_manager::init was called?",
                     int(sid_.app_));
  }

  const node_to_proc_map& node_map = it1->second;
  auto it2 = node_map.find(addr);
  if (it2 == node_map.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "process_manager::get_partner: "
                     "Could not find nodeaddress %ld for app %d",
                     long(addr), int(sid_.app_));
  }

  return it2->second;
}

}
}