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

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_NODE_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NODE_NODE_H_INCLUDED

#include <sstmac/software/ami/ami.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/hardware/common/unique_id.h>
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

class node :
  public failable,
  public connectable_component
{
  DeclareFactory(node,uint64_t,event_manager*)
 public:
  void setup() override;

  void init(unsigned int phase) override;

  virtual ~node();

  void
  connect_output(sprockit::sim_parameters* params,
                 int src_outport, int dst_inport,
                 event_handler* handler) override;

  void
  connect_input(sprockit::sim_parameters* params,
                 int src_outport, int dst_inport,
                 event_handler* handler) override;


  link_handler*
  payload_handler(int port) const override;

  link_handler*
  credit_handler(int port) const override;

  /**
   @return  The object encapsulating the memory model
  */
  memory_model*
  mem() const {
    return mem_model_;
  }

  processor*
  proc() const {
    return proc_;
  }

  /**
   @return  A handler wrapper for scheduling events to the NIC
  */
  nic*
  get_nic() const {
    return nic_;
  }

  /**
   @return  The operating system managing apps on this node
  */
  sw::operating_system*
  os() const {
    return os_;
  }

  /**
   @return  A unique string description of the node
  */
  virtual std::string
  to_string() const override;

  /**
   @return  A unique integer identifier
  */
  node_id
  addr() const {
    return my_addr_;
  }

  int
  nsocket() const {
    return nsocket_;
  }

  int
  launch_root() const {
    return launch_root_;
  }

  /**
   Cause the node to crash. This cancels all future events for this node.
  */
  void fail_stop();

  /**
   Choose a unique (64-bit) integer ID for a message. This will never be reused
   except for integer overflow.
   @return A unique 64-bit integer
  */
  unique_event_id
  allocate_unique_id() {
    return next_outgoing_id_++;
  }

  /**
   Make the node execute a particular compute function. This
   generally causes the function to be executed immediately.
   @param func  Enum identifying the type of computation
   @param data  Event object encapsulating data/metadata for computation
   @param cb    The event to execute when kernel is complete
  */
  virtual void
  execute(ami::COMP_FUNC func,
           event* data,
           callback* cb) = 0;

  /**
   * @brief execute Asynchronously execute a kernel on some
   * service associated with the node. This generally enqueues an operation
   * to be performed - not necessarily executing it immediately.
   * @param func
   * @param data
   */
  virtual void
  execute(ami::SERVICE_FUNC func,
                 event* data);

  void handle(event* ev);

  /**
   Push a network message (operation at the MTL layer) onto the NIC
   @param netmsg
  */
  void send_to_nic(network_message* netmsg);

  void deadlock_check() override;

  void increment_app_refcount();

  void decrement_app_refcount();

 protected:
  node(sprockit::sim_parameters* params,
    uint64_t id,
    event_manager* mgr);

 protected:
  sw::operating_system* os_;

  node_id my_addr_;

  memory_model* mem_model_;

  processor* proc_;

  nic* nic_;
  
  int nsocket_;

 private:
  int app_refcount_;
  int launch_root_;

  sw::app_launcher* app_launcher_;
  sw::job_launcher* job_launcher_;

  unique_event_id next_outgoing_id_;
  sprockit::sim_parameters* params_;

};

}
} // end of namespace sstmac

#endif