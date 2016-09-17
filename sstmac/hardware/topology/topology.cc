#include <sstmac/hardware/topology/topology.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::hw::topology);
RegisterNamespaces("topology");

RegisterDebugSlot(topology,
    "debug all operations performed by topology objects such as connections in the network or routing computations");

namespace sstmac {
namespace hw {

topology* topology::static_topology_ = 0;
topology* topology::main_top_ = 0;
const int topology::eject = -1;

topology::topology(sprockit::sim_parameters* params,
                   InitMaxPortsIntra i1,
                   InitGeomEjectID i2) :
  max_ports_intra_network_(-1),
  eject_geometric_id_(-1),
  max_ports_injection_(-1),
  endpoints_per_switch_(-1),
  rng_(nullptr)
{
  concentration_ = params->get_optional_int_param("concentration",1);
  endpoints_per_switch_ = concentration_;

  injection_redundancy_ = params->get_optional_int_param("injection_redundant", 1);
  max_ports_injection_ = endpoints_per_switch_;
  eject_geometric_id_ = max_ports_intra_network_ + max_ports_injection_;

  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  num_nodes_per_endpoint_ = netlink_params->get_optional_int_param("radix", 1);

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
    static_topology_ = topology_factory::get_param("name", top_params);
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

void
topology::minimal_route_to_node(
  switch_id current_sw_addr,
  node_id dest_node_addr,
  routable::path& path) const
{
  abort();
  int dir;
  switch_id ej_addr = endpoint_to_ejection_switch(dest_node_addr, dir);
  if (ej_addr == current_sw_addr) {
    path.outport = eject_port(dir);
    path.vc = 0;
  }
  else {
    minimal_route_to_switch(current_sw_addr, ej_addr, path);
  }
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

void
topology::nodes_connected_to_ejection_switch(switch_id swaddr,
                                   std::vector<node_id>& nodes) const
{
  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    nodes[i] = netlink_id(swaddr*concentration_ + i);
  }
}

void
topology::nodes_connected_to_injection_switch(switch_id swaddr,
                                   std::vector<node_id>& nodes) const
{
  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    nodes[i] = netlink_id(swaddr*concentration_ + i);
  }
}

void
topology::build_internal_connectables(internal_connectable_map &connectables,
  connectable_factory factory,
  connectable_factory dummy_factory,
  partition *part, int my_rank,
  sprockit::sim_parameters *params)
{
  int n_switches = num_switches();
  for (int i=0; i < n_switches; ++i){
    switch_id sid(i);
    top_debug("Switch %d belongs to rank %d for building NICs: my_rank=%d",
      i, part->lpid_for_switch(sid), my_rank);
    if (part->lpid_for_switch(sid) == my_rank){
      params->add_param_override("id", i);
      connectables[sid] = factory(params, i);
    } else {
      connectables[sid] = dummy_factory(params, i);
    }
  }
}

void
topology::connect_end_point_objects(
  sprockit::sim_parameters* ej_params,
  sprockit::sim_parameters* inj_params,
  internal_connectable_map& internal,
  end_point_connectable_map& end_points)
{


  for (auto& pair : end_points) {
    connectable* node = pair.second;
    node_id nodeaddr = pair.first;

    //map to topology-specific port
    int num_ports;
    int ports[32];
    switch_id injaddr = endpoint_to_injection_switch(nodeaddr, ports, num_ports);
    connectable* injsw = internal[injaddr];

    for (int i=0; i < num_ports; ++i){
      int injector_port = i;
      int switch_port = ports[i];
      top_debug("connecting switch %d to injector %d on ports %d:%d",
          int(injaddr), int(nodeaddr), switch_port, injector_port);
      injsw->connect_input(ej_params, injector_port, switch_port, node);
      node->connect_output(inj_params, injector_port, switch_port, injsw);
    }

    switch_id ejaddr = endpoint_to_ejection_switch(nodeaddr, ports, num_ports);
    connectable* ejsw = internal[ejaddr];

    for (int i=0; i < num_ports; ++i){
      int ejector_port = i;
      int switch_port = ports[i];
      top_debug("connecting switch %d to ejector %d on ports %d:%d",
          int(ejaddr), int(nodeaddr), switch_port, ejector_port);
      ejsw->connect_output(ej_params, switch_port, ejector_port, node);
      node->connect_input(inj_params, switch_port, ejector_port, ejsw);
    }

  }
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
  return params->get_namespace(sprockit::printf("port%d", port));
}

void
topology::create_partition(
  int* switches_per_lp,
  int *switch_to_lp,
  int *switch_to_thread,
  int& local_num_switches,
  int me,
  int nproc,
  int nthread,
  int noccupied)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "topology::partition: not valid for %s",
    to_string().c_str());
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
topology::endpoint_label(node_id nid) const
{
  netlink_id netid(nid / num_nodes_per_endpoint_);
  return label(netid);
}

std::string
topology::label(node_id nid) const
{
  return sprockit::printf("%d", int(nid));
}

std::string
topology::label(switch_id sid) const
{
  return sprockit::printf("%d", int(sid));
}

std::string
topology::label(event_loc_id id) const
{
  if (id.is_node_id()){
    return label(id.convert_to_node_id());
  }
  else {
    return label(id.convert_to_switch_id());
  }
}

}
}

