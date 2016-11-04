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

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NIC_NETWORKINTERFACE_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NIC_NETWORKINTERFACE_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/common/failable.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/common/packet_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/common/stats/stat_histogram_fwd.h>
#include <sstmac/common/stats/stat_local_int_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/common/messages/sst_message_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>

DeclareDebugSlot(nic);

#define nic_debug(...) \
  debug_printf(sprockit::dbg::nic, "NIC on node %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

/**
 * A networkinterface is a delegate between a node and a server module.
 * This object helps ornament network operations with information about
 * the process (ppid) involved.
 */
class nic :
  public failable,
  public connectable_subcomponent
{

 public:
  typedef enum {
    Injection,
    LogP
  } Port;

  virtual std::string
  to_string() const override = 0;

  virtual ~nic();

  /**
   * @return A unique ID for the NIC positions. Opaque typedef to an int.
   */
  node_id
  addr() const {
    return my_addr_;
  }

  /**
   * Set an event handler wrapper encapsulation the parent computational unit.
   * @param nd The compute node attached to the NIC
   */
  virtual void
  set_node(node* nd){
    parent_ = nd;
  }

  event_handler*
  mtl_handler() const {
    return event_mtl_handler_;
  }

  void mtl_handle(event* ev);

  /**
   * Delete all static variables associated with this class.
   * This should be registered with the runtime system via need_delete_statics
   */
  static void
  delete_statics();

  /**
    Perform the set of operations standard to all NICs.
    This then passes control off to a model-specific #do_send
    function to actually carry out the send
    @param payload The network message to send
  */
  void
  internode_send(network_message* payload);

  /**
    Perform the set of operations standard to all NICs
    for transfers within a node. This function is model-independent,
    unlike #internode_send which must pass control to #do_send.
   * @param payload
   */
  void
  intranode_send(network_message* payload);

  void
  send_to_logp_switch(network_message* netmsg);

 protected:
  nic(sprockit::sim_parameters* params, node* parent);

  node*
  parent() const {
    return parent_;
  }

  /**
    Start the message sending and inject it into the network
    This performs all model-specific work
    @param payload The network message to send
  */
  virtual void
  do_send(network_message* payload) = 0;

  void
  send_to_node(network_message* netmsg);

  bool
  negligible_size(int bytes) const {
    return bytes <= negligible_size_;
  }

  /**
   The NIC can either receive an entire message (bypass the byte-transfer layer)
   or it can receive packets.  If an incoming message is a full message (not a packet),
   it gets routed here. Unlike #recv_chunk, this has a default implementation and does not throw.
   @param chunk
   */
  void
  recv_message(message* msg);

 protected:
  node_id my_addr_;

  int negligible_size_;

  node* parent_;

  event_handler* logp_switch_;
  event_handler* event_mtl_handler_;
  event_handler* node_handler_;


#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* link_mtl_handler_;
#endif

 private:
  stat_spyplot* spy_num_messages_;
  stat_spyplot* spy_bytes_;
  stat_histogram* hist_msg_size_;
  stat_local_int* local_bytes_sent_;
  stat_global_int* global_bytes_sent_;

 private:
  /**
   For messages requiring an NIC ACK to signal that the message
   has injected into the interconnect.  Create an ack and
   send it up to the parent node.
   */
  void
  ack_send(network_message* payload);

  void record_message(network_message* msg);


};

DeclareFactory(nic,node*);

}
} // end of namespace sstmac.

#endif

