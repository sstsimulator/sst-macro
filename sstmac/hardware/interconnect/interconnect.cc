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

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/statics.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

RegisterDebugSlot(interconnect);

namespace sstmac {
namespace hw {

Interconnect* Interconnect::static_interconnect_ = nullptr;

Interconnect*
Interconnect::staticInterconnect(SST::Params& params, EventManager* mgr)
{
  if (!static_interconnect_){
    ParallelRuntime* rt = ParallelRuntime::staticRuntime(params);
    Partition* part = rt ? rt->topologyPartition() : nullptr;
    static_interconnect_ = new Interconnect(params, mgr, part, rt);
  }
  return static_interconnect_;
}

Interconnect*
Interconnect::staticInterconnect()
{
  if (!static_interconnect_){
    spkt_abort_printf("interconnect not initialized");
  }
  return static_interconnect_;
}

#if !SSTMAC_INTEGRATED_SST_CORE
Interconnect::~Interconnect()
{
  for (auto* nd : nodes_) if (nd) delete nd;
  for (auto* sw : logp_switches_) if (sw) delete sw;
  for (auto* sw : switches_) if (sw) delete sw;
}
#endif

Interconnect::Interconnect(SST::Params& params, EventManager *mgr,
                           Partition *part, ParallelRuntime *rt)
{
  if (!static_interconnect_) static_interconnect_ = this;
  topology_ = Topology::staticTopology(params);
  num_nodes_ = topology_->numNodes();
  num_switches_ = topology_->numSwitches();
  num_leaf_switches_ = topology_->numLeafSwitches();
  Runtime::setTopology(topology_);

#if !SSTMAC_INTEGRATED_SST_CORE
  components_.resize(topology_->numNodes() + topology_->numSwitches());

  partition_ = part;
  rt_ = rt;
  int nproc = rt_->nproc();
  num_speedy_switches_with_extra_node_ = num_nodes_ % nproc;
  num_nodes_per_speedy_switch_ = num_nodes_ / nproc;

  SST::Params node_params = params.get_namespace("node");
  SST::Params nic_params = node_params.get_namespace("nic");
  SST::Params switch_params = params.get_namespace("switch");

  Topology* top = topology_;

  std::string switch_model = switch_params->getLowercaseParam("name");
  bool logp_model = switch_model == "logp" || switch_model == "simple" || switch_model == "macrels";

  switches_.resize(num_switches_);
  nodes_.resize(num_nodes_);

  SST::Params logp_params;
  if (logp_model){
    logp_params.insert(switch_params);
  }
  logp_params.insert(switch_params.find_scoped_params("logp"));

  logp_switches_.resize(rt_->nthread());
  uint32_t my_offset = rt_->me() * rt_->nthread() + top->numNodes() + top->numSwitches();
  for (int i=0; i < rt_->nthread(); ++i){
    uint32_t id = my_offset + i;
    logp_switches_[i] = new LogPSwitch(id, logp_params);
  }

  interconn_debug("Interconnect building endpoints");

  buildEndpoints(node_params, nic_params, mgr);

  connectLogP(mgr, node_params, nic_params);
  if (!logp_model){
    interconn_debug("Interconnect building switches");
    buildSwitches(switch_params, mgr);
    interconn_debug("Interconnect connecting switches");
    connectSwitches(mgr, switch_params);
    interconn_debug("Interconnect connecting endpoints");
    connectEndpoints(mgr, nic_params, switch_params);
    configureInterconnectLookahead(params);
  } else {
    //lookahead is actually higher
    LogPSwitch* lsw = logp_switches_[0];
    lookahead_ = lsw->out_in_latency();
  }

  Timestamp lookahead_check = lookahead_;
  if (EventLink::minRemoteLatency().ticks() > 0){
    lookahead_check = EventLink::minRemoteLatency();
  }
  if (EventLink::minThreadLatency().ticks() > 0){
    lookahead_check = std::min(lookahead_check, EventLink::minThreadLatency());
  }

  if (lookahead_check < lookahead_){
    spkt_abort_printf("invalid lookahead compute: computed lookahead to be %8.4e, "
        "but have link with lookahead %8.4e", lookahead_.sec(), lookahead_check.sec());
  }

#endif
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
Interconnect::configureInterconnectLookahead(SST::Params& params)
{
  SST::Params switch_params = params.get_namespace("switch");
  SST::Params inj_params = params.get_namespace("node")
      .find_scoped_params("nic").find_scoped_params("injection");

  SST::Params link_params = switch_params.get_namespace("link");
  Timestamp hop_latency(link_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());
  Timestamp injection_latency = Timestamp(inj_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());

  lookahead_ = std::min(injection_latency, hop_latency);
}
#endif

SwitchId
Interconnect::nodeToLogpSwitch(NodeId nid) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return topology_->nodeToLogpSwitch(nid);
#else
  SwitchId real_sw_id = topology_->endpointToSwitch(nid);
  int target_rank = partition_->lpidForSwitch(real_sw_id);
  return target_rank;
#endif
}


#if !SSTMAC_INTEGRATED_SST_CORE

#if 0
EventLink*
Interconnect::allocateIntraProcLink(Timestamp latency, EventManager* mgr, EventHandler* handler,
                                EventScheduler* src, EventScheduler* dst)
{
  EventLink* iplink = nullptr;
  if (src->threadId() == dst->threadId()){
    iplink = new LocalLink(latency,mgr,handler, src->componentId(), dst->componentId());
  } else {
    iplink = new MultithreadLink(handler,latency,mgr,dst,
                               src->componentId(), dst->componentId());
  }
  links_.push_back(iplink);
  return iplink;
}
#endif

void
Interconnect::connectEndpoints(EventManager* mgr,
                               SST::Params& ep_params,
                               SST::Params& sw_params)
{
  int num_nodes = topology_->numNodes();
  int num_switches = topology_->numSwitches();
  int me = rt_->me();
  std::vector<Topology::InjectionPort> ports;
  SST::Params inj_params = ep_params.find_scoped_params("injection");
  SST::Params ej_params = ep_params.find_scoped_params("ejection");
  SST::Params link_params= sw_params.find_scoped_params("link");
  Timestamp inj_latency(inj_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());
  Timestamp ej_latency;
  if (ej_params.contains("latency")){
    ej_latency = Timestamp(ej_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());
  } else {
    ej_latency = Timestamp(link_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());
  }



  for (int i=0; i < num_switches; ++i){
    //parallel - I don't own this
    int target_rank = partition_->lpidForSwitch(i);
    int target_thread = partition_->threadForSwitch(i);
    if (target_rank != me && target_thread != mgr->thread()){
      continue;
    }

    NetworkSwitch* injsw = switches_[i];
    NetworkSwitch* ejsw = switches_[i];

    topology_->endpointsConnectedToInjectionSwitch(i, ports);
    for (Topology::InjectionPort& p : ports){
      Node* ep = nodes_[p.nid];

      interconn_debug("connecting switch %d:%p to injector %d:%p on ports %d:%d",
          i, injsw, p.nid, ep, p.switch_port, p.ep_port);

      auto credit_link = new LocalLink(inj_latency, mgr, ep->creditHandler(p.ep_port));
      injsw->connectInput(p.ep_port, p.switch_port, EventLink::ptr(credit_link));
      auto payload_link = new LocalLink(inj_latency, mgr, injsw->payloadHandler(p.switch_port));
      ep->connectOutput(p.ep_port, p.switch_port, EventLink::ptr(payload_link));
    }

    topology_->endpointsConnectedToEjectionSwitch(i, ports);
    for (Topology::InjectionPort& p : ports){
      Node* ep = nodes_[p.nid];

      interconn_debug("connecting switch %d:%p to ejector %d:%p on ports %d:%d",
          int(i), ejsw, p.nid, ep, p.switch_port, p.ep_port);

      auto payload_link = new LocalLink(ej_latency, mgr, ep->payloadHandler(p.ep_port));
      ejsw->connectOutput(p.switch_port, p.ep_port, EventLink::ptr(payload_link));

      auto credit_link = new LocalLink(ej_latency, mgr, ejsw->creditHandler(p.switch_port));
      ep->connectInput(p.switch_port, p.ep_port, EventLink::ptr(credit_link));
    }
  }

}

void
Interconnect::setup()
{
  for (Node* node : nodes_){
    if (node){
      node->init(0);
      node->setup();
    }
  }

  for (NetworkSwitch* sw : switches_){
    if (sw){
      sw->init(0);
      sw->setup();
    }
  }

  for (LogPSwitch* sw : logp_switches_){
    if (sw){
      sw->init(0);
      sw->setup();
    }
  }
}

void
Interconnect::connectLogP(EventManager* mgr,
  SST::Params& node_params,
  SST::Params& nic_params)
{
  SST::Params inj_params = nic_params.get_namespace("injection");
  SST::Params empty{};

  int my_rank = rt_->me();
  int my_thread = mgr->thread();

  LogPSwitch* local_logp_switch = logp_switches_[my_thread];
  Timestamp logp_link_latency = local_logp_switch->out_in_latency();
  for (int i=0; i < num_switches_; ++i){
    SwitchId sid(i);
    std::vector<Topology::InjectionPort> nodes;
    topology_->endpointsConnectedToInjectionSwitch(sid, nodes);
    if (nodes.empty())
      continue;

    int target_thread = partition_->threadForSwitch(i);
    int target_rank = partition_->lpidForSwitch(sid);

    for (Topology::InjectionPort& conn : nodes){
      Node* nd = nodes_[conn.nid];
      if (my_rank == target_rank && my_thread == target_thread){
        //nic sends to only its specific logp switch
        auto* logp_link = new LocalLink(Timestamp(0), mgr,
            local_logp_switch->payloadHandler(conn.switch_port));
        nd->nic()->connectOutput(NIC::LogP, conn.switch_port, EventLink::ptr(logp_link));

        auto* out_link = new LocalLink(logp_link_latency, mgr, nd->payloadHandler(NIC::LogP));
        local_logp_switch->connectOutput(conn.nid, EventLink::ptr(out_link));
      } else if (my_rank == target_rank){
        auto* out_link = new MultithreadLink(logp_link_latency,
            mgr, EventManager::global->threadManager(target_thread), nd->payloadHandler(NIC::LogP));
        local_logp_switch->connectOutput(conn.nid, EventLink::ptr(out_link));
      } else {
        auto* out_link = new IpcLink(logp_link_latency, target_rank, mgr,
                                          local_logp_switch->componentId(), nodeComponentId(conn.nid), NIC::LogP, false);
        local_logp_switch->connectOutput(conn.nid, EventLink::ptr(out_link));
      }
    }
  }
}

void
Interconnect::buildEndpoints(SST::Params& node_params,
                  SST::Params& nic_params,
                  EventManager* mgr)
{
  int my_rank = rt_->me();
  int my_thread = mgr->thread();

  for (int i=0; i < num_switches_; ++i){
    SwitchId sid(i);
    std::vector<Topology::InjectionPort> nodes;
    topology_->endpointsConnectedToInjectionSwitch(sid, nodes);
    if (nodes.empty())
      continue;
    int target_thread = partition_->threadForSwitch(i);
    int target_rank = partition_->lpidForSwitch(sid);
    interconn_debug("switch %d maps to target rank %d", i, target_rank);

    for (int n=0; n < nodes.size(); ++n){
      NodeId nid = nodes[n].nid;
      int ep_port = nodes[n].ep_port;
      int sw_port = nodes[n].switch_port;
      interconn_debug("building node %d on leaf switch %d", nid, i);

      if (my_rank == target_rank || my_thread == target_thread){
        //local node - actually build it
        node_params->addParamOverride("id", int(nid));
        uint32_t comp_id = nid;
        auto nodeType = node_params.find<std::string>("name", "simple");
        auto pos = nodeType.find("_node"); //append the node prefix if missing
        if (pos == std::string::npos){
          nodeType = nodeType + "_node";
        }
        Node* nd = sprockit::create<Node>(
             "macro", nodeType, comp_id, node_params);
        node_params->removeParam("id"); //you don't have to let it linger
        nodes_[nid] = nd;
        components_[nid] = nd;
      }
    }
  }
}

void
Interconnect::buildSwitches(SST::Params& switch_params,
                            EventManager* mgr)
{
  bool simple_model = switch_params.find<std::string>("name") == "simple";
  if (simple_model) return; //nothing to do

  int my_rank = rt_->me();
  int id_offset = topology_->numNodes();
  for (SwitchId i=0; i < num_switches_; ++i){
    switch_params->addParamOverride("id", int(i));
    if (partition_->lpidForSwitch(i) == my_rank){
      int thread = partition_->threadForSwitch(i);
      uint32_t comp_id = switchComponentId(i);
      auto swType = switch_params.find<std::string>("name");
      auto pos = swType.find("_switch"); //append the switch prefix if missing
      if (pos == std::string::npos){
        swType = swType + "_switch";
      }
      switches_[i] = sprockit::create<NetworkSwitch>(
         "macro", swType, comp_id, switch_params);
    } else {
      switches_[i] = nullptr;
    }
    switch_params->removeParam("id");
    components_[i+id_offset] = switches_[i];
  }
}

uint32_t
Interconnect::switchComponentId(SwitchId sid) const
{
  return topology_->numNodes() + sid;
}

uint32_t
Interconnect::nodeComponentId(NodeId nid) const
{
  return nid;
}

void
Interconnect::connectSwitches(EventManager* mgr, SST::Params& switch_params)
{
  bool simple_model = switch_params.find<std::string>("name") == "simple";
  if (simple_model) return; //nothing to do

  std::vector<Topology::Connection> outports(64); //allocate 64 spaces optimistically

  int my_rank = rt_->me();
  int my_thread = mgr->thread();

  SST::Params port_params = switch_params.get_namespace("link");
  Timestamp linkLatency(port_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());

  for (int i=0; i < num_switches_; ++i){
    interconn_debug("interconnect: connecting switch %i", i);
    SwitchId src(i);
    int src_rank = partition_->lpidForSwitch(i);
    int src_thread = partition_->threadForSwitch(i);
    topology_->connectedOutports(src, outports);
    for (Topology::Connection& conn : outports){
      int dst_rank = partition_->lpidForSwitch(conn.dst);
      int dst_thread = partition_->threadForSwitch(conn.dst);

      interconn_debug("%s connecting to %s on ports %d:%d",
                topology_->switchLabel(src).c_str(),
                topology_->switchLabel(conn.dst).c_str(),
                conn.src_outport, conn.dst_inport);

      if (src_rank == my_rank && src_thread == my_thread){
        EventLink* payload_link = nullptr;
        if (dst_rank == my_rank && dst_thread == my_thread){
          payload_link = new LocalLink(linkLatency, mgr, switches_[conn.dst]->payloadHandler(conn.dst_inport));
        } else if (dst_rank == my_rank){
          payload_link = new MultithreadLink(linkLatency, mgr, EventManager::global->threadManager(dst_thread),
               switches_[conn.dst]->payloadHandler(conn.dst_inport));
        } else {
          payload_link = new IpcLink(linkLatency, dst_rank, mgr,
                                     switchComponentId(src), switchComponentId(conn.dst),
                                     conn.dst_inport, false);
        }
        switches_[src]->connectOutput(conn.src_outport, conn.dst_inport, EventLink::ptr(payload_link));

      }

      if (dst_rank == my_rank && dst_thread == my_thread){
        EventLink* credit_link = nullptr;
        if (src_rank == my_rank && src_thread == my_thread){
          credit_link = new LocalLink(linkLatency, mgr, switches_[src]->creditHandler(conn.src_outport));
        } else if (src_rank == my_rank) {
          credit_link = new MultithreadLink(linkLatency, mgr, EventManager::global->threadManager(src_thread),
               switches_[src]->creditHandler(conn.src_outport));
        } else {
          credit_link = new IpcLink(linkLatency, src_rank, mgr,
                                    switchComponentId(conn.dst), switchComponentId(src),
                                    conn.src_outport, true);
        }
        switches_[conn.dst]->connectInput(conn.src_outport, conn.dst_inport, EventLink::ptr(credit_link));
      }
    }
  }
}
#endif

}
}
