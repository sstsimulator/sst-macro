/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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

#include <sstmac/hardware/topology/topology.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <fstream>

#if SSTMAC_INTEGRATED_SST_CORE && SSTMAC_HAVE_VALID_MPI
#include <mpi.h>
#endif

RegisterNamespaces("topology");

RegisterKeywords(
{ "topology_name", "DEPRECATED: name of the topology" },
{ "topology_geometry", "DEPRECATED: an array specifying the geometry of the topology" },
{ "topology_redundant", "DEPRECATED: for group-based topologies (dragonfly), number of group connections per router" },
{ "topology_seed", "DEPRECATED: a seed for random number generators used by topology" },
{ "name", "the name of the topology" },
{ "geometry", "an array specifying the geometry of the topology" },
{ "redundant", "an array specifying how many redundants links in certain dimensions of topology" },
{ "seed", "a seed for random number generators used by topology" },
{ "concentration", "the number of nodes per switch" },
{ "network_nodes_per_switch", "DEPRECATED: the number of nodes per switch" },
{ "auto", "whether to auto-generate topology based on app size"},
{ "output_graph", "enable dot format topology graph generation by specifying an output filename"},
);

RegisterDebugSlot(topology,
    "debug all operations performed by topology objects such as connections in the network or routing computations");

namespace sstmac {
namespace hw {

Topology* Topology::staticTopology_ = nullptr;
Topology* Topology::main_top_ = nullptr;

#if SSTMAC_INTEGRATED_SST_CORE
int Topology::nproc = 0;

SwitchId
Topology::nodeToLogpSwitch(NodeId nid) const
{
  // currently using single switch topology for short circuit network when using sst-core
  // and I think the code below is wrong for some corner cases anyways

  spkt_abort_printf("unexpected call to nodeToLogPSwitch()");
  return 0;

  // leaving previous implementation here for potential future use
//  int n_nodes = numNodes();
//  int nodes_per_switch = n_nodes / nproc;
//  int epPlusOne = nodes_per_switch + 1;
//  int num_procs_with_extra_node = n_nodes % nproc;

//  int div_cutoff = num_procs_with_extra_node * epPlusOne;
//  if (nid >= div_cutoff){
//    int offset = nid - div_cutoff;
//    return offset / nodes_per_switch;
//  } else {
//    return nid / epPlusOne;
//  }
}
#endif

Topology::Topology(SST::Params& params)
{
#if SSTMAC_INTEGRATED_SST_CORE
#if SSTMAC_HAVE_VALID_MPI
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
#else
  nproc = 1;
#endif
#endif
  main_top_ = this;

  dot_file_ = params.find<std::string>("output_graph", "");
  xyz_file_ = params.find<std::string>("outputXYZ", "");

  if (params.contains("dump_file")){
    dump_file_ = params.find<std::string>("dump_file");
  }

  if (params.contains("routing_tables")){
    std::string router_fname = params.find<std::string>("routing_tables");
    std::ifstream rin(router_fname);
    nlohmann::json rtr_jsn;
    try {
      rin >> rtr_jsn;
    } catch (nlohmann::detail::exception& e) {
      spkt_abort_printf("failed parsing json file %s", router_fname.c_str());
    }
    routing_tables_ = rtr_jsn.at("switches");
  }
}

Topology::~Topology()
{
}

Topology*
Topology::staticTopology(SST::Params& params)
{
  if (!staticTopology_){
    SST::Params top_params = params.get_scoped_params("topology");
    std::string name = top_params.find<std::string>("name");
    if (name.empty()){
      spkt_abort_printf("no topology.name parameter in namespace");
    }
    staticTopology_ = sprockit::create<Topology>("macro", name, top_params);
  }
  return staticTopology_;
}

std::string
Topology::getPortNamespace(int port)
{
  return std::string("port") + std::to_string(port);
}

static std::string get_outfile(const std::string& cmd_line_given,
                               const std::string& input_file_given)
{
  if (!cmd_line_given.empty()) return cmd_line_given; //cmd takes precedence
  else return input_file_given;
}

void
Topology::outputGraphviz(const std::string& path)
{
  std::string output = get_outfile(path, dot_file_);
  if (output.empty()) return;

  std::ofstream out(output);
  out << "graph {\n";

  int nsw = numSwitches();
  for (int s=0; s < nsw; ++s){
    std::string lbl = switchLabel(s);
    out << "sw" << s << " [style=filled,fillcolor=\"lightblue\",shape=rect,label=\""
                << lbl << "\"];\n";
  }

  std::vector<Connection> conns;
  std::map<int, int> weighted_conns;
  out << "\nedge[];\n";
  for (int s=0; s < nsw; ++s){
    connectedOutports(s, conns);
    weighted_conns.clear();
    for (Connection& c : conns){
      weighted_conns[c.dst] += 1;
    }
    for (auto& pair : weighted_conns){
      int dst = pair.first;
      if (s < dst){
        out << "sw" << s << "--sw" << dst << ";\n";
      }
    }
  }

  out << "\n}";
  out.close();
}

void
Topology::outputBox(std::ostream& os,
                     const Topology::VTKBoxGeometry& box)
{
  os << box.vertex(0);
  for (int i=1; i < 8; ++i){
    os << "->" << box.vertex(i);
  }
}

void
Topology::outputBox(std::ostream& os,
                     const Topology::VTKBoxGeometry& box,
                     const std::string& color,
                     const std::string& alpha)
{
  outputBox(os, box);
  os << ";color=" << color;
  os << ";alpha=" << alpha;
}

void
Topology::outputXYZ(const std::string& path)
{
  std::string output = get_outfile(path, xyz_file_);
  if (output.empty()) return;

  int nsw = numSwitches();
  //int half = nsw / 2;
  std::ofstream out(output);

  for (int sid=0; sid < nsw; ++sid){
    VTKSwitchGeometry geom = getVtkGeometry(sid);
    outputBox(out, geom.box, "gray", "0.1"); //very transparent
    out << "\n";
  }
  out.close();
}

void
Topology::createPartition(
  int * /*switch_to_lp*/,
  int * /*switch_to_thread*/,
  int  /*me*/,
  int  /*nproc*/,
  int  /*nthread*/,
  int  /*noccupied*/) const
{
  spkt_throw_printf(sprockit::UnimplementedError,
    "topology::partition: not valid for %s",
    toString().c_str());
}

CartesianTopology*
Topology::cartTopology() const
{
  sprockit::abort("topology::cartTopology: cannot cast to cartesian topology");
  return nullptr;
}

std::string
Topology::nodeLabel(NodeId nid) const
{
  return sprockit::sprintf("%d", nid);
}


std::string
Topology::switchLabel(SwitchId sid) const
{
  return sprockit::sprintf("%d", sid);
}

std::string
Topology::label(uint32_t comp_id) const
{
  if (comp_id < numNodes()){
    return nodeLabel(comp_id);
  } else {
    return switchLabel(comp_id - numNodes());
  }
}

Topology::VTKSwitchGeometry
Topology::getVtkGeometry(SwitchId  /*sid*/) const
{
  spkt_abort_printf("unimplemented: topology::getVtkGeometry for %s",
                    toString().c_str());
  return VTKSwitchGeometry(0,0,0,0,0,0,0,std::vector<VTKSwitchGeometry::port_geometry>());
}

std::string
Topology::nodeIdToName(NodeId i)
{
  if (i >= hostmap_.size()){
    spkt_abort_printf("Invalid node id %d given to topology::nodeIdToName", i)
  }
  return hostmap_[i];
}

NodeId
Topology::nodeNameToId(const std::string& hostname) const
{
  auto it = idmap_.find(hostname);
  if (it == idmap_.end()){
    spkt_abort_printf("topology: can't find %s in hostname map",
                      hostname.c_str());
  }
  return it->second;
}

void
Topology::initHostnameMap(SST::Params&  /*params*/)
{
  if (!idmap_.empty() || !hostmap_.empty()){
    spkt_abort_printf("topology::initHostnameMap: maps not empty");
  }

  int nn = numNodes();
  hostmap_.resize(nn);

  for (int i=0; i < nn; ++i) {
    std::string name = std::string("nid") + std::to_string(i);
    idmap_[name] = i;
    hostmap_[i] = name;
  }
}

void
Topology::dumpPorts()
{
  if (!dump_file_.empty()){
    std::cout << "Dumping topology port configuration to " << dump_file_ << std::endl;
    portConfigDump(dump_file_);
  }
}

void
Topology::portConfigDump(const std::string & /*dumpFile*/)
{
  spkt_abort_printf("Topology chosen does not support port dump");
}

void
Topology::injectionPorts(NodeId nid, std::vector<InjectionPort>& node_ports)
{
  node_ports.clear();
  SwitchId sid = endpointToSwitch(nid);
  std::vector<InjectionPort> switch_ports;
  endpointsConnectedToEjectionSwitch(sid, switch_ports);
  for (InjectionPort& switch_port : switch_ports){
    if (switch_port.nid == nid){
      node_ports.push_back(switch_port);
    }
  }
}

class MerlinTopology : public Topology {

 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    MerlinTopology,
    "macro",
    "merlin",
    "a dummy topology for running with Merlin")

  MerlinTopology(SST::Params& params)
    : Topology(params)
  {
    num_nodes_ = params.find<int>("num_nodes");
    num_switches_ = params.find<int>("num_switches");
  }

  std::string toString() const override {
    return "merlin topology";
  }

  SwitchId numSwitches() const override {
    return num_switches_;
  }

  NodeId numNodes() const override {
    return num_nodes_;
  }

  SwitchId endpointToSwitch(NodeId) const override {
    spkt_abort_printf("merlin topology functions should never be called");
    return 0;
  }

  void connectedOutports(SwitchId  /*src*/, std::vector<Topology::Connection>&  /*conns*/) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  int maxNumPorts() const override {
    spkt_abort_printf("merlin topology functions should never be called");
    return -1;
  }

  int numHopsToNode(NodeId  /*src*/, NodeId  /*dst*/) const override {
    spkt_abort_printf("merlin topology functions should never be called");
    return -1;
  }

  void endpointsConnectedToInjectionSwitch(SwitchId  /*swid*/,
                          std::vector<InjectionPort>&  /*nodes*/) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void endpointsConnectedToEjectionSwitch(SwitchId  /*swid*/,
                          std::vector<InjectionPort>&  /*nodes*/) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  SwitchId maxSwitchId() const override {
    spkt_abort_printf("merlin topology functions should never be called");
    return -1;
  }

  NodeId maxNodeId() const override {
    spkt_abort_printf("merlin topology functions should never be called");
    return -1;
  }

 private:
  int num_nodes_;
  int num_switches_;
};

}
}
