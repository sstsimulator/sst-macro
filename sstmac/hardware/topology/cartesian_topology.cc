#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
  namespace hw {

cartesian_topology::cartesian_topology(sprockit::sim_parameters *params,
                                       InitMaxPortsIntra i1,
                                       InitGeomEjectID i2) :
  structured_topology(params, i1, i2)
{
  params->get_vector_param("geometry", dimensions_);
  if (dimensions_.size() == 0) {
    spkt_throw_printf(sprockit::value_error, "empty topology vector for hdtorus");
  }

  if (params->has_param("redundant")) {
    params->get_vector_param("redundant", red_);
    if (red_.size() != dimensions_.size()) {
      spkt_throw_printf(sprockit::input_error,
                       "topology::init: wrong number of dimensions in topology_redundant, "
                       "should be %d, got %d\n",
                       dimensions_.size(),
                       red_.size());
    }
  }
  else {
    int ndim = dimensions_.size();
    red_.resize(ndim);
    for (int i = 0; i < ndim; i++) {
      red_[i] = 1;
    }
  }
}

void
cartesian_topology::minimal_routes_to_switch(switch_id current_sw_addr,
                                             switch_id dest_sw_addr,
                                             structured_routable::path& current_path,
                                             structured_routable::path_set& paths) const
{
  coordinates src = switch_coords(current_sw_addr);
  coordinates dst = switch_coords(dest_sw_addr);
  minimal_routes_to_coords(src, dst, current_path, paths);
}

  }
}
