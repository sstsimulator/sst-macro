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

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NIC_NETWORKINTERFACE_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NIC_NETWORKINTERFACE_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/common/failable.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/common/packet_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/hardware/logp/logp_switch_fwd.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/common/stats/stat_histogram_fwd.h>
#include <sstmac/hardware/common/flow_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/progress_queue.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/hardware/topology/topology_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factory.h>

#include <functional>

DeclareDebugSlot(nic);

#define nic_debug(...) \
  debug_printf(sprockit::dbg::nic, "NIC on node %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

class NicEvent :
  public Event, public sprockit::thread_safe_new<NicEvent>
{
  ImplementSerializable(NicEvent)
 public:
  NicEvent(NetworkMessage* msg) : msg_(msg) {}

  NetworkMessage* msg() const {
    return msg_;
  }

#if !SSTMAC_INTEGRATED_SST_CORE
  void validate_serialization(serializable* ser) override;
#endif

  void serialize_order(serializer& ser) override;

 private:
  NicEvent(){} //for serialization

  NetworkMessage* msg_;
};

/**
 * A networkinterface is a delegate between a node and a server module.
 * This object helps ornament network operations with information about
 * the process (ppid) involved.
 */
class NIC : public ConnectableSubcomponent
{
 public:
#if !SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_DECLARE_BASE(NIC)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(SST::Component*,SST::Params&)
#endif

  typedef enum {
    Injection,
    LogP
  } Port;

  virtual std::string toString() const override = 0;

  virtual ~NIC();

  /**
   * @return A unique ID for the NIC positions. Opaque typedef to an int.
   */
  NodeId addr() const {
    return my_addr_;
  }

  Topology* topology() const {
    return top_;
  }

  /**
   * @brief injectSend Perform an operation on the NIC.
   *  This assumes an exlcusive model of NIC use. If NIC is busy,
   *  operation may complete far in the future. If wishing to query for how busy the NIC is,
   *  use #next_free. Calls to hardware taking an OS parameter
   *  indicate 1) they MUST occur on a user-space software thread
   *  and 2) that they should us the os to block and compute
   * @param netmsg The message being injected
   * @param os     The OS to use form software compute delays
   */
  void injectSend(NetworkMessage* netmsg);

  EventHandler* mtlHandler() const;

  virtual void mtlHandle(Event* ev);

  /**
   * Delete all static variables associated with this class.
   * This should be registered with the runtime system via need_deleteStatics
   */
  static void deleteStatics();

  /**
    Perform the set of operations standard to all NICs.
    This then passes control off to a model-specific #doSend
    function to actually carry out the send
    @param payload The network message to send
  */
  void internodeSend(NetworkMessage* payload);

  /**
    Perform the set of operations standard to all NICs
    for transfers within a node. This function is model-independent,
    unlike #internodeSend which must pass control to #doSend.
   * @param payload
   */
  void intranodeSend(NetworkMessage* payload);

  /**
   The NIC can either receive an entire message (bypass the byte-transfer layer)
   or it can receive packets.  If an incoming message is a full message (not a packet),
   it gets routed here. Unlike #recv_chunk, this has a default implementation and does not throw.
   @param chunk
   */
  void recvMessage(NetworkMessage* msg);

  void sendToNode(NetworkMessage* netmsg);

  virtual void sendManagerMsg(NetworkMessage* msg);

  virtual std::function<void(NetworkMessage*)> ctrlIoctl();

  virtual std::function<void(NetworkMessage*)> dataIoctl();

 protected:
  NIC(SST::Component* parent, SST::Params& params);

  Node* parent() const {
    return parent_;
  }

  /**
    Start the message sending and inject it into the network
    This performs all model-specific work
    @param payload The network message to send
  */
  virtual void doSend(NetworkMessage* payload) = 0;

  bool negligibleSize(int bytes) const {
    return bytes <= negligibleSize_;
  }

  int negligibleSize_;
  Node* parent_;
  NodeId my_addr_;
  EventLink::ptr logp_link_;
  Topology* top_;

 private:
  StatSpyplot<int,int,uint64_t>* spy_bytes_;
  Statistic<uint64_t>* xmit_flows_;
  sw::SingleProgressQueue<NetworkMessage> queue_;

 protected:
  sw::OperatingSystem* os_;

 private:
  /**
   For messages requiring an NIC ACK to signal that the message
   has injected into the interconnect.  Create an ack and
   send it up to the parent node.
   */
  void ackSend(NetworkMessage* payload);

  void recordMessage(NetworkMessage* msg);

  void finishMemcpy(NetworkMessage* msg);

};

class NullNIC : public NIC
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT(
    NullNIC,
    "macro",
    "null_nic",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a nic that models nothing - stand-in only",
    "nic")
#else
  SST_ELI_REGISTER_DERIVED(
    NIC,
    NullNIC,
    "macro",
    "null",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a nic that models nothing - stand-in only")
#endif

  NullNIC(SST::Component* parent, SST::Params& params) :
    NIC(parent, params)
  {
  }

  std::string toString() const override { return "null nic"; }

  void doSend(NetworkMessage* msg) override {}

  void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& payload_link) override {}

  void connectInput(int src_outport, int dst_inport, EventLink::ptr&& credit_link) override {}

  LinkHandler* payloadHandler(int port) override { return nullptr; }

  LinkHandler* creditHandler(int port) override { return nullptr; }
};

}
} // end of namespace sstmac.

#endif
