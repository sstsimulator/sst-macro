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

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/delete.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::hw::router);
RegisterDebugSlot(router);
RegisterDebugSlot(routing);
RegisterNamespaces("router");

namespace sstmac {
namespace hw {

router::~router()
{
}

int
router::convert_to_port(int dim, int dir)
{
  return top_->convert_to_port(dim, dir);
}

void
router::set_switch(network_switch* netsw)
{
  netsw_ = netsw;
}

void
router::finalize_init()
{
}

void
router::init_vc()
{
  max_num_vc_ = 0;
  top_->configure_vc_routing(num_vc_lookup_);
  std::map<routing::algorithm_t, int>::iterator it, end = num_vc_lookup_.end();
  for (it=num_vc_lookup_.begin(); it != end; ++it){
    int nvc = it->second;
    max_num_vc_ = std::max(max_num_vc_, nvc);
  }
}

router::router() : max_num_vc_(0)
{
}

bool
router::productive_paths_to_node(node_id dst,
                                 geometry_routable::path_set &paths)
{
  int ej_port;
  switch_id ej_addr = top_->endpoint_to_ejection_switch(dst, ej_port);
  if (ej_addr == my_addr_) {
    paths.resize(1);
    paths[0].outport = ej_port;
    //paths[0].dim = topology::eject;
    //paths[0].dir = ej_port;
    paths[0].vc = 0;
    return true;
  }
  else {
    productive_paths_to_switch(ej_addr, paths);
    return false;
  }
}

void
router::minimal_route_to_node(node_id node_addr, geometry_routable::path& path)
{
  spkt_throw(sprockit::unimplemented_error,
            to_string(),
            "::minimal_route_to_node: router does not implement path routing");
}

void
router::minimal_route_to_switch(switch_id node_addr, geometry_routable::path& path)
{
  spkt_throw(sprockit::unimplemented_error,
            to_string(),
            "::minimal_route_to_switch: router does not implement path routing");
}

routing::algorithm_t
router::str_to_algo(const std::string &str)
{
  if (str == "minimal") {
    return routing::minimal;
  }
  else if (str == "valiant") {
    return routing::valiant;
  }
  else if (str == "min_ad") {
    return routing::minimal_adaptive;
  }
  else if (str == "ugal") {
    return routing::ugal;
  }
  else {
    spkt_throw_printf(sprockit::input_error,
                     "invalid routing algorithm %s",
                     str.c_str());
  }
}

void
router::init_factory_params(sprockit::sim_parameters* params)
{
  my_addr_ = switch_id(params->get_int_param("id"));
  /**
    sstkeyword {
      docstring=Enables hop count reporting.ENDL
      If set to true, warnings will be provided each time a hop count increases by a given number.
      This can only be enabled if sanity check is enabled by configure.;
    }
  */
  hop_count_reporting_ =
      params->get_optional_bool_param("sanity_check_hop_count_reporting",false);
  /**
    sstkeyword {
      docstring=Sets the count delta for hop count reporting.ENDL
      The default is 100 hops (which may be entirely inapporpriate for some topologies).;
    }
  */
  hop_count_delta_ =
      params->get_optional_int_param("sanity_check_hop_count_delta", 100);
}

}
}


