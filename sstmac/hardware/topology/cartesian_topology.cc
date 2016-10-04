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

coordinates
cartesian_topology::node_coords(node_id uid) const
{
  if (concentration_ == 1) {
    return switch_coords((switch_id)uid);
  }
  else {
    switch_id swid(uid / concentration_);
    int lastidx = uid % concentration_;
    coordinates coords = switch_coords(swid);
    coords.push_back(lastidx);
    return coords;
  }
}

node_id
cartesian_topology::node_addr(const coordinates& coords) const
{

  int offset = 0;
  if (coords.size() > ndimensions()) {
    //there is no "extra" switch index
    offset = coords[ndimensions()];
  }

  int swid = switch_addr(coords);
  node_id nid = swid * concentration_ + offset;
  return nid;
}

std::string
cartesian_topology::node_label(node_id nid) const
{
  return node_coords(nid).to_string();
}

std::string
cartesian_topology::switch_label(switch_id sid) const
{
  return switch_coords(sid).to_string();
}

  }
}
