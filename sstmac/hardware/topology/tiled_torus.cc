
#include <sstmac/hardware/topology/tiled_torus.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("tiled_torus | tiled_hdtorus", topology, tiled_torus);

void
tiled_torus::init_factory_params(sprockit::sim_parameters *params)
{
  hdtorus::init_factory_params(params);
  ntiles_row_ = params->get_int_param("tiles_per_row");
  ntiles_col_ = params->get_int_param("tiles_per_col");

  int ntiles = ntiles_col_ * ntiles_row_;

  max_ports_intra_network_ = ntiles - max_ports_injection_;

  int ndims = red_.size();
  tile_offsets_.resize(ndims, 0);
  rotater_.resize(ndims, 0);

  tile_offsets_[0] = 0;
  for (int i=1; i < ndims; ++i){
    //times 2 - pos and neg
    tile_offsets_[i] = red_[i-1]*2 + tile_offsets_[i-1];
  }

  eject_geometric_id_ = 2*ndims;

  //allocate the last row to injection/ejection
}

void
tiled_torus::minimal_routes_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  structured_routable::path& current,
  structured_routable::path_set &paths) const
{
  hdtorus::minimal_route_to_coords(src_coords, dest_coords, current);
  int dim = current.outport / 2; //2 for +/-
  int dir = current.outport % 2;
  int red = red_[dim];
  paths.resize(red);
  int port_offset = tile_offsets_[dim] + red * dir;

  top_debug("routing from %s to %s: adding %d paths starting from port %d",
     src_coords.to_string().c_str(),
     dest_coords.to_string().c_str(),
     red, port_offset);

  //outport identifies a unique path
  for (int r=0; r < red; ++r){
    paths[r] = current;
    paths[r].geometric_id = current.outport;
    paths[r].outport = port_offset + r;
  }
}

int
tiled_torus::port(int replica, int dim, int dir)
{
  int offset = tile_offsets_[dim];
  //offset for dim, each replica has +/- for *2
  return offset + red_[dim] * dir + replica;
}

void
tiled_torus::connect_dim(int dim,
  connectable *center,
  connectable *plus,
  connectable *minus)
{
  //times 2 for +/-1
  int nreplica = red_[dim];
  int outport, inport;
  connectable::config cfg;
  cfg.ty = connectable::BasicConnection;
  for (int r=0; r < nreplica; ++r){
    outport = inport = port(r, dim, hdtorus::pos);
    top_debug("\tconnecting + replica %d on port %d", r, outport);
    center->connect(outport, inport, connectable::output, plus, &cfg);
    plus->connect(outport, inport, connectable::input, center, &cfg);

    outport = inport = port(r, dim, hdtorus::neg);
    top_debug("\tconnecting + replica %d on port %d", r, outport);
    center->connect(outport, inport, connectable::output, minus, &cfg);
    minus->connect(outport, inport, connectable::input, center, &cfg);
  }
}

switch_id
tiled_torus::endpoint_to_ejection_switch(
    node_id nodeaddr, int ports[], int &num_ports) const
{
  return endpoint_to_injection_switch(nodeaddr, ports, num_ports);
}

switch_id
tiled_torus::endpoint_to_injection_switch(
    node_id nodeaddr, int ports[], int &num_ports) const
{
  int port;
  //this computes the port assuming one injection path
  switch_id sid = hdtorus::endpoint_to_injection_switch(nodeaddr, port);
  int offset = port - max_ports_intra_network_;

  port = max_ports_intra_network_ + offset*injection_redundancy_;
  num_ports = injection_redundancy_;
  for (int i=0; i < num_ports; ++i, ++port){
    top_debug("switch %d sending back injection port %d:%d to node %d",
       int(sid), i, port, int(nodeaddr));
    ports[i] = port;
  }
  return sid;
}

void
tiled_torus::eject_paths_on_switch(
  node_id dest_addr, switch_id sw_addr,
  structured_routable::path_set &paths) const
{
  int node_offset = dest_addr % endpoints_per_switch_;
  int port = max_ports_intra_network_ + node_offset*injection_redundancy_;
  for (int i=0; i < injection_redundancy_; ++i, ++port){
    paths[i].outport = port;
    paths[i].vc = 0;
    paths[i].geometric_id = 2*dimensions_.size() + node_offset;
  }
}

}
}
