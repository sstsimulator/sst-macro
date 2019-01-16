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

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_NODE_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_NODE_H_INCLUDED

#include <sstmac/software/ami/ami.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/hardware/common/unique_id.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/common/failable.h>

#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>
#include <sstmac/software/launch/app_launcher_fwd.h>
#include <sstmac/software/launch/launch_event_fwd.h>
#include <sstmac/software/libraries/service_fwd.h>
#include <sstmac/hardware/nic/nic_fwd.h>
#include <sstmac/hardware/memory/memory_model_fwd.h>
#include <sstmac/hardware/processor/processor_fwd.h>

#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(node);
#define node_debug(...) \
  debug_printf(sprockit::dbg::node, "Node %d: %s", int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

class Node : public ConnectableComponent
{
  DeclareFactory(Node,uint32_t)
 public:
  void setup() override;

  void init(unsigned int phase) override;

  virtual ~Node();

  void connectOutput(SST::Params& params,
                 int src_outport, int dst_inport,
                 EventLink* link) override;

  void connectInput(SST::Params& params,
                 int src_outport, int dst_inport,
                 EventLink* link) override;

  Timestamp sendLatency(SST::Params& params) const override;

  Timestamp creditLatency(SST::Params& params) const override;

  LinkHandler* payloadHandler(int port) override;

  LinkHandler* creditHandler(int port) override;

  /**
   @return  The object encapsulating the memory model
  */
  MemoryModel* mem() const {
    return mem_model_;
  }

  Processor* proc() const {
    return proc_;
  }

  /**
   @return  A handler wrapper for scheduling events to the NIC
  */
  NIC* nic() const {
    return nic_;
  }

  /**
   @return  The operating system managing apps on this node
  */
  sw::OperatingSystem* os() const {
    return os_;
  }

  /**
   @return  A unique string description of the node
  */
  virtual std::string toString() const override;

  /**
   @return  A unique integer identifier
  */
  NodeId addr() const {
    return my_addr_;
  }

  int nsocket() const {
    return nsocket_;
  }

  int launchRoot() const {
    return launchRoot_;
  }

  /**
   Choose a unique (64-bit) integer ID for a message. This will never be reused
   except for integer overflow.
   @return A unique 64-bit integer
  */
  UniqueEventId allocateUniqueId() {
    return next_outgoing_id_++;
  }

  /**
   Make the node execute a particular compute function. This
   generally causes the function to be executed immediately.
   @param func  Enum identifying the type of computation
   @param data  Event object encapsulating data/metadata for computation
   @param cb    The event to execute when kernel is complete
  */
  virtual void execute(ami::COMP_FUNC func,
           Event* data, ExecutionEvent* cb) = 0;

  /**
   * @brief execute Asynchronously execute a kernel on some
   * service associated with the node. This generally enqueues an operation
   * to be performed - not necessarily executing it immediately.
   * @param func
   * @param data
   */
  virtual void execute(ami::SERVICE_FUNC func, Event* data);

  void handle(Event* ev);

  void incrementAppRefcount();

  void decrementAppRefcount();

 protected:
  Node(SST::Params& params, uint32_t id);

 protected:
  sw::OperatingSystem* os_;

  NodeId my_addr_;

  MemoryModel* mem_model_;

  Processor* proc_;

  NIC* nic_;
  
  int nsocket_;

 private:
  int app_refcount_;
  int launchRoot_;

  sw::AppLauncher* app_launcher_;
  sw::JobLauncher* job_launcher_;

  UniqueEventId next_outgoing_id_;
  SST::Params params_;

};

}
} // end of namespace sstmac

#endif
