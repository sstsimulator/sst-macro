/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#include <sstmac/hardware/node/simple_node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/libraries/compute/compute_event.h>

#if SSTMAC_HAVE_SST_ELEMENTS
#include <sst/elements/ariel/arielnotify.h>
#endif

#include <sprockit/errors.h>
#include <sprockit/util.h>

#include <iostream>

#include <unusedvariablemacro.h>

namespace sstmac {
namespace hw {


SimpleNode::~SimpleNode()
{
}

SimpleNode::SimpleNode(uint32_t id, SST::Params& params)
  : Node(id, params)
{
  initLinks(params);

#if SSTMAC_HAVE_SST_ELEMENTS
  int ncores = proc_->ncores();
  unblock_links_.resize(ncores);
  for (int i=0; i < ncores; ++i){
    std::string linkName = "unblock" + std::to_string(i);
    unblock_links_[i] = configureLink(linkName, new Event::Handler<SimpleNode>(this, &SimpleNode::unblock));
  }
#endif
}

void
SimpleNode::init(SSTMAC_MAYBE_UNUSED unsigned int phase)
{
#if SSTMAC_HAVE_SST_ELEMENTS
  if (phase == 1){
    SST::Link* link0 = unblock_links_[0];
    if (link0){
      Event* ev = link0->recvInitData();
      auto* nev = dynamic_cast<SST::ArielComponent::NameEvent*>(ev);
      os_->setIpcName(nev->name());
    }
  }
#endif
}

void
SimpleNode::execute(ami::COMP_FUNC func, Event* data, ExecutionEvent* cb)
{
  node_debug("executing kernel %s on node %d",
             ami::tostr(func), my_addr_);
  switch (func) {
    case sstmac::ami::COMP_INSTR:
      proc_->compute(data, cb);
      break;
    case sstmac::ami::COMP_TIME: {
      sw::TimedComputeEvent* ev = safe_cast(sw::TimedComputeEvent, data);
      sendDelayedExecutionEvent(ev->data(), cb);
      break;
    }
    default:
      spkt_throw_printf(sprockit::SpktError,
            "simplenode: cannot process kernel %s",
            ami::tostr(func));
  }
}

void
SimpleNode::unblock(SSTMAC_MAYBE_UNUSED Event *ev)
{
#if SSTMAC_HAVE_SST_ELEMENTS
  auto* nev = dynamic_cast<SST::ArielComponent::NotifyEvent*>(ev);
  os_->unblockBlockedThread();
#endif
}

}
} // end of namespace sstmac.
