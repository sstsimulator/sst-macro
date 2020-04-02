/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_SIMPLENODE_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_SIMPLENODE_H_INCLUDED

#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/processor/processor_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>

namespace sstmac {
namespace hw {

/**
 * A stand-in node.  This node type passes all requests directly to
 * the simulator and has no resource contention (infinite processors),
 */
class SimpleNode :
  public Node
{
 public:
  SST_ELI_REGISTER_DERIVED_COMPONENT(
    Node,
    SimpleNode,
    "macro",
    "simple_node",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Basic processor that only does timed_message computes",
    COMPONENT_CATEGORY_NETWORK)

  SST_ELI_DOCUMENT_PORTS(
    SSTMAC_VALID_PORTS,
    {"unblock%(core)d", "receives unblock events from other components", {}}
  )

  SST_ELI_DOCUMENT_STATISTICS(
    {"xmit_wait", "stalled cycles with data but no credits", "nanoseconds", 1},
    {"xmit_bytes", "number of bytes transmitted on a port", "bytes", 1},
    {"xmit_flows", "number of bytes sent as network flows", "bytes", 1},
    {"recv_bytes", "number of bytes receive on a port", "bytes", 1},
    {"spy_bytes", "a spyplot of the bytes sent", "bytes", 1},
    {"otf2", "Write an OTF2 trace", "n/a", 1},
    {"delays", "Statistic for tracking individual message delays", "n/a", 1},
    {"xmit_stall", "congestion stalls", "cycles", 1},
    {"xmit_active", "activity statistic", "cycles", 1}, // Name, Desc, Units, Enable Level
    {"xmit_idle", "idle statistic", "cycles", 1}, // Name, Desc, Units, Enable Level
    {"bytes_sent", "data sent on port", "bytes", 1}
  )

  SimpleNode(uint32_t id, SST::Params& params);

  ~SimpleNode();

  void execute(ami::COMP_FUNC func,
         Event* data, ExecutionEvent* cb) override;

  void unblock(Event* ev);

  void init(unsigned int phase) override;
 private:
#if SSTMAC_HAVE_SST_ELEMENTS
  std::vector<SST::Link*> unblock_links_;
#endif
};


}
} // end of namespace sstmac

#endif
