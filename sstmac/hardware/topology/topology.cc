#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/router/structured_router.h>
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

topology::topology() :
  max_ports_intra_network_(-1),
  max_ports_injection_(-1),
  endpoints_per_switch_(-1),
  rng_(0)
{
}

topology::~topology()
{
  if (rng_) delete rng_;
}

coordinates
topology::switch_coords(switch_id swid) const
{
  spkt_throw(sprockit::unimplemented_error,
    "topology::switch_coords: current topology does not implement coordinate system");
}

void
topology::sanity_check()
{
  if (max_ports_intra_network_ < 0){
    spkt_throw_printf(sprockit::value_error,
      "topology::max_radix_ uninitialized");
  }
  if (max_ports_injection_ < 0){
    spkt_throw_printf(sprockit::value_error,
      "topology::max_nps_ uninitialized");
  }
}

void
topology::finalize_init()
{
  sanity_check();
}

topology*
topology::static_topology(sprockit::sim_parameters* params)
{
  if (!static_topology_){
    sprockit::sim_parameters* top_params = params->top_parent()->get_namespace("topology");
    static_topology_ = topology_factory::get_param("name", top_params);
  }
  return static_topology_;
}

void
topology::init_factory_params(sprockit::sim_parameters* params)
{
  outputgraph_ = params->get_optional_bool_param("output_graph", false);

  netlink_endpoints_ = params->get_optional_bool_param("netlink_endpoints", false);
  num_nodes_per_netlink_ = params->get_optional_int_param("netlink_radix", 1);

  if (netlink_endpoints_){
    endpoints_per_switch_ /= num_nodes_per_netlink_;
  } else {
    num_nodes_per_netlink_ = 1;
  }

  /**
    sstkeyword {
      gui = 42;
      docstring = In some cases, walking the topology requires a random decision.ENDL
      The seed for all the random decisions can be explicitly set for debugging.;
      extra = true;
    }
  */
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

uint32_t
topology::random_number(uint32_t max, uint32_t attempt) const
{
#if SSTMAC_USE_MULTITHREAD
  static thread_lock lock;
  lock.lock();
#endif
  if (debug_seed_){
    std::vector<RNG::rngint_t> seeds(2);
    uint32_t time = event_manager::global ? event_manager::global->now().msec() : 42;
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
  geometry_routable::path& path) const
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
topology::connect_end_point_objects(
  internal_connectable_map& internal,
  end_point_connectable_map& end_points)
{
  end_point_connectable_map::iterator it, end = end_points.end();
  int the_only_port = 0;
  connectable::config cfg;
  cfg.ty = connectable::BasicConnection;
  for (it = end_points.begin(); it != end; it++) {
    connectable* node = it->second;
    node_id nodeaddr = it->first;

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
      injsw->connect(injector_port, switch_port, connectable::input, node, &cfg);
      node->connect(injector_port, switch_port, connectable::output, injsw, &cfg);
    }

    switch_id ejaddr = endpoint_to_ejection_switch(nodeaddr, ports, num_ports);
    connectable* ejsw = internal[ejaddr];

    for (int i=0; i < num_ports; ++i){
      int ejector_port = i;
      int switch_port = ports[i];
      top_debug("connecting switch %d to ejector %d on ports %d:%d",
          int(ejaddr), int(nodeaddr), switch_port, ejector_port);
      ejsw->connect(switch_port, ejector_port, connectable::output, node, &cfg);
      node->connect(switch_port, ejector_port, connectable::input, ejsw, &cfg);
    }

  }
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
  netlink_id netid(nid / num_nodes_per_netlink_);
  return label(netid);
}

std::string
topology::label(node_id nid) const
{
  return sprockit::printf("node(%d)", int(nid));
}

std::string
topology::label(switch_id sid) const
{
  return sprockit::printf("switch(%d)", int(sid));
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

