
#include <sstmac/hardware/topology/tiled_torus.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("tiled_torus | tiled_hdtorus", topology, tiled_torus);

tiled_torus::tiled_torus(sprockit::sim_parameters *params) :
  hdtorus(params)
{
  ntiles_row_ = params->get_int_param("tiles_per_row");
  ntiles_col_ = params->get_int_param("tiles_per_col");

  int ntiles = ntiles_col_ * ntiles_row_;

  first_simple_torus_eject_port_ = max_ports_intra_network_;

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
tiled_torus::get_redundant_paths(
  routable::path& current,
  routable::path_set &paths) const
{
  if (current.outport < first_simple_torus_eject_port_){
    //intranetwork routing
    int dim = current.outport / 2; //2 for +/-
    int dir = current.outport % 2;
    int red = red_[dim];
    paths.resize(red);
    int port_offset = tile_offsets_[dim] + red * dir;

    //outport identifies a unique path
    for (int r=0; r < red; ++r){
      paths[r] = current;
      paths[r].geometric_id = current.outport;
      paths[r].outport = port_offset + r;
    }
  } else {
    //ejection routing
    int offset = current.outport - first_simple_torus_eject_port_;
    int port = max_ports_intra_network_ + offset*injection_redundancy_;
    int num_ports = injection_redundancy_;
    for (int i=0; i < num_ports; ++i, ++port){
      paths[i].outport = port;
    }
  }
}

switch_id
tiled_torus::endpoint_to_injection_switch(node_id nodeaddr, int ports[], int &num_ports) const
{
  int port;
  switch_id sid = hdtorus::endpoint_to_injection_switch(nodeaddr, port);
  //ejection routing
  int offset = port - first_simple_torus_eject_port_;
  port = max_ports_intra_network_ + offset*injection_redundancy_;
  num_ports = injection_redundancy_;
  for (int i=0; i < num_ports; ++i, ++port){
    ports[i] = port;
  }
  return sid;
}

void
tiled_torus::configure_geometric_paths(std::vector<int>& redundancies) const
{
  int ndims = dimensions_.size();
  int ngeom_paths = ndims * 2 + endpoints_per_switch_; //2 for +/-
  redundancies.resize(ngeom_paths);
  for (int d=0; d < ndims; ++d){
    int pos_path = hdtorus::convert_to_port(d,pos);
    redundancies[pos_path] = red_[d];

    int neg_path = hdtorus::convert_to_port(d,neg);
    redundancies[neg_path] = red_[d];
  }

  for (int i=0; i < endpoints_per_switch_; ++i){
    redundancies[i+eject_geometric_id_] = injection_redundancy_;
  }
}

void
tiled_torus::connect_dim(
  sprockit::sim_parameters* params,
  int dim,
  connectable *center,
  connectable *plus,
  connectable *minus)
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");


  //times 2 for +/-1
  int nreplica = red_[dim];
  int outport, inport;

  for (int r=0; r < nreplica; ++r){
    outport = inport = port(r, dim, hdtorus::pos);
    top_debug("\tconnecting + replica %d on port %d", r, outport);
    center->connect_output(link_params, outport, inport, plus);
    plus->connect_input(link_params, outport, inport, center);

    outport = inport = port(r, dim, hdtorus::neg);
    top_debug("\tconnecting + replica %d on port %d", r, outport);
    center->connect_output(link_params, outport, inport, minus);
    minus->connect_input(link_params, outport, inport, center);
  }
}

}
}
