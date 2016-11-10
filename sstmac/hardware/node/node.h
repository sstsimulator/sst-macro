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
#include <sstmac/software/launch/app_launch_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>
#include <sstmac/software/launch/launcher_fwd.h>
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

  void schedule_launches();

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
  void build_launchers(sprockit::sim_parameters* params);

 private:
  int app_refcount_;
  sw::app_launcher* app_launcher_;
  sw::job_launcher* job_launcher_;
  static std::list<sw::app_launch*> app_launchers_;
  unique_event_id next_outgoing_id_;
  sprockit::sim_parameters* params_;

};

DeclareFactory(node,uint64_t,event_manager*);

}
} // end of namespace sstmac

#endif

