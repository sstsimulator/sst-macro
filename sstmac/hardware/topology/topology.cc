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

topology::topology(sprockit::sim_parameters* params) :
  rng_(nullptr)
{
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
  int* switches_per_lp,
  int *switch_to_lp,
  int *switch_to_thread,
  int& local_num_switches,
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
    int port = port_start + nports;
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

