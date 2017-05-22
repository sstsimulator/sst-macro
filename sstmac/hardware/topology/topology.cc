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

#include <sstmac/hardware/topology/topology.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#if SSTMAC_INTEGRATED_SST_CORE && SSTMAC_HAVE_MPI_H
#include <mpi.h>
#endif

RegisterNamespaces("topology");

RegisterKeywords(
"topology_name",
"topology_geometry",
"topology_group_connections",
"topology_true_random_intermediate",
"topology_redundant",
"topology_seed",
"topology_output_graph",
"geometry",
"redundant",
"seed",
"group_connections",
"concentration",
"network_nodes_per_switch",
);

RegisterDebugSlot(topology,
    "debug all operations performed by topology objects such as connections in the network or routing computations");

namespace sstmac {
namespace hw {

topology* topology::static_topology_ = nullptr;
topology* topology::main_top_ = nullptr;
const int topology::eject = -1;

#if SSTMAC_INTEGRATED_SST_CORE
int topology::nproc = 0;

switch_id
topology::node_to_logp_switch(node_id nid) const
{
  int n_nodes = num_nodes();
  int netlinks_per_switch = n_nodes / nproc;
  int epPlusOne = netlinks_per_switch + 1;
  int num_procs_with_extra_node = n_nodes % nproc;

  int div_cutoff = num_procs_with_extra_node * epPlusOne;
  if (nid >= div_cutoff){
    int offset = nid - div_cutoff;
    return offset / netlinks_per_switch;
  } else {
    return nid / epPlusOne;
  }
}
#endif

topology::topology(sprockit::sim_parameters* params) :
  rng_(nullptr)
{
#if SSTMAC_INTEGRATED_SST_CORE
#if SSTMAC_HAVE_MPI_H
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
#else
  nproc = 1;
#endif
#endif
  std::vector<RNG::rngint_t> seeds(2);
  seeds[0] = 42;
  if (params->has_param("seed")) {
    seed_ = params->get_long_param("seed");
    seeds[1] = seed_;
    debug_seed_ = true;
  } else {
    seeds[1] = time(NULL);
    debug_seed_ = false;
  }
  rng_ = RNG::MWC::construct(seeds);

  main_top_ = this;
}

topology::~topology()
{
  if (rng_) delete rng_;
}

topology*
topology::static_topology(sprockit::sim_parameters* params)
{
  if (!static_topology_){
    sprockit::sim_parameters* top_params = params->get_namespace("topology");
    static_topology_ = topology::factory::get_param("name", top_params);
  }
  return static_topology_;
}

uint32_t
topology::random_number(uint32_t max, uint32_t attempt) const
{
#if SSTMAC_USE_MULTITHREAD
  static thread_lock lock;
  lock.lock();
#endif
  if (debug_seed_){
    std::vector<RNG::rngint_t> seeds(2);
    uint32_t time = 42;
    seeds[1] = seed_ * (time+31) << (attempt + 5);
    seeds[0] = (time+5)*7 + seeds[0]*attempt*42 + 3;
    rng_->vec_reseed(seeds);
  } 
  uint32_t result = rng_->value_in_range(max);
#if SSTMAC_USE_MULTITHREAD
  lock.unlock();
#endif
  return result;
}

switch_id
topology::random_intermediate_switch(switch_id current, switch_id dest)
{
  static thread_lock lock;
  lock.lock();  //need to be thread safe
  long nid = current;
  uint32_t attempt = 0;
  while (current == nid) {
    nid = random_number(num_switches(), attempt); 
    ++attempt;
  }
  lock.unlock();
  return switch_id(nid);
}

sprockit::sim_parameters*
topology::setup_port_params(int port, int credits, double bw,
                            sprockit::sim_parameters* link_params,
                            sprockit::sim_parameters* params)
{
  std::string port_name = sprockit::printf("port%d", port);
  sprockit::sim_parameters* port_params = params->get_optional_namespace(port_name);
  //for max lookahead, no credit latency
  //put all of the credits on sending, none on credits
  (*port_params)["bandwidth"].setBandwidth(bw/1e9, "GB/s");
  (*port_params)["credits"].setByteLength(credits, "B");
  port_params->add_param_override("send_latency", link_params->get_param("send_latency"));
  port_params->add_param_override("credit_latency", link_params->get_param("credit_latency"));
  port_params->add_param_override("arbitrator", link_params->get_param("arbitrator"));
  return port_params;
}

sprockit::sim_parameters*
topology::get_port_params(sprockit::sim_parameters *params, int port)
{
  return params->get_optional_namespace(sprockit::printf("port%d", port));
}

void
topology::create_partition(
  int *switch_to_lp,
  int *switch_to_thread,
  int me,
  int nproc,
  int nthread,
  int noccupied) const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "topology::partition: not valid for %s",
    to_string().c_str());
}

void
topology::configure_individual_port_params(int port_start, int nports,
                                           sprockit::sim_parameters *switch_params) const
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  for (int i=0; i < nports; ++i){
    int port = port_start + i;
    sprockit::sim_parameters* port_params = get_port_params(switch_params, port);
    link_params->combine_into(port_params);
  }
}

cartesian_topology*
topology::cart_topology() const
{
  spkt_throw(sprockit::value_error, "topology is not a cartesian topology");
}

void
topology::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1;
  m[routing::minimal_adaptive] = 1;
  m[routing::valiant] = 1;
  m[routing::ugal] = 1;
}

std::string
topology::node_label(node_id nid) const
{
  return sprockit::printf("%d", nid);
}


std::string
topology::switch_label(switch_id sid) const
{
  return sprockit::printf("%d", sid);
}

std::string
topology::label(device_id id) const
{
  if (id.is_node_id()){
    return node_label(id.id());
  } else {
    return switch_label(id.id());
  }

}

class merlin_topology : public topology {
  FactoryRegister("merlin", topology, merlin_topology)
 public:
  merlin_topology(sprockit::sim_parameters* params) 
    : topology(params)
  {
    num_nodes_ = params->get_int_param("num_nodes");
    num_switches_ = params->get_int_param("num_switches");
  }

  std::string to_string() const override {
    return "merlin topology";
  }

  int num_switches() const override {
    return num_switches_;
  }

  int num_nodes() const override {
    return num_nodes_;
  }

  bool uniform_network_ports() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  bool uniform_switches() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void connected_outports(switch_id src, std::vector<topology::connection>& conns) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void configure_individual_port_params(switch_id src,
          sprockit::sim_parameters* switch_params) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }


  int num_netlinks() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  int max_num_ports() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  switch_id netlink_to_injection_switch(node_id nodeaddr, int& switch_port) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  switch_id netlink_to_ejection_switch(node_id nodeaddr, int& switch_port) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void configure_vc_routing(std::map<routing::algorithm_t, int>& m) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  switch_id node_to_ejection_switch(node_id addr, int& port) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  switch_id node_to_injection_switch(node_id addr, int& port) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  int minimal_distance(switch_id src, switch_id dst) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  int num_hops_to_node(node_id src, node_id dst) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void nodes_connected_to_injection_switch(switch_id swid,
                          std::vector<injection_port>& nodes) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void nodes_connected_to_ejection_switch(switch_id swid,
                          std::vector<injection_port>& nodes) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  switch_id max_switch_id() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  bool switch_id_slot_filled(switch_id sid) const override{
    spkt_abort_printf("merlin topology functions should never be called");
  }

  netlink_id max_netlink_id() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  bool netlink_id_slot_filled(netlink_id sid) const override{
    spkt_abort_printf("merlin topology functions should never be called");
  }


  node_id max_node_id() const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  bool node_id_slot_filled(node_id nid) const override{
    spkt_abort_printf("merlin topology functions should never be called");
  }

  void minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

  bool node_to_netlink(node_id nid, node_id& net_id, int& offset) const override {
    spkt_abort_printf("merlin topology functions should never be called");
  }

 private:
  int num_nodes_;
  int num_switches_;
};

}
}