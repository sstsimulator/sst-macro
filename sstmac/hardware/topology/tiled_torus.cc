
#include <sstmac/hardware/topology/tiled_torus.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"tiles_per_row",
"tiles_per_col",
"tiles_per_inj",
"injection_ports",
);

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
tiled_torus::netlink_to_injection_switch(node_id nodeaddr, int ports[], int &num_ports) const
{
  int port;
  switch_id sid = hdtorus::netlink_to_injection_switch(nodeaddr, port);
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
tiled_torus::configure_individual_port_params(switch_id src,
                                       sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, max_ports_intra_network_, switch_params);
}

void
tiled_torus::configure_geometric_paths(std::vector<int>& redundancies) const
{
  int ndims = dimensions_.size();
  int ngeom_paths = ndims * 2 + netlinks_per_switch_; //2 for +/-
  redundancies.resize(ngeom_paths);
  for (int d=0; d < ndims; ++d){
    int pos_path = hdtorus::convert_to_port(d,pos);
    redundancies[pos_path] = red_[d];

    int neg_path = hdtorus::convert_to_port(d,neg);
    redundancies[neg_path] = red_[d];
  }

  for (int i=0; i < netlinks_per_switch_; ++i){
    redundancies[i+eject_geometric_id_] = injection_redundancy_;
  }
}

void
tiled_torus::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  conns.resize(max_ports_intra_network_ + max_ports_injection_);
  int cidx = 0;
  int ndims = dimensions_.size();
  int dim_stride = 1;
  for (int i=0; i < ndims; ++i){
    int plus_jump = 1;
    int minus_jump = -1;
    int srcX = (src / dim_stride) % dimensions_[i];
    int last_row = dimensions_[i] - 1;
    if (srcX == last_row){
      //wrap around
      plus_jump = -last_row;
    } else if (srcX == 0){
      minus_jump = last_row;
    }

    switch_id plus_partner = src + plus_jump * dim_stride;

    switch_id minus_partner = src + minus_jump * dim_stride;

    int nreplica = red_[i];
    int outport, inport;
    for (int r=0; r < nreplica; ++r){
      outport = inport = port(r, i, hdtorus::pos);
      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = plus_partner;
      conn.src_outport = outport;
      conn.dst_inport = inport;
      ++cidx;

      outport = inport = port(r, i, hdtorus::neg);
      conn = conns[cidx];
      conn.src = src;
      conn.dst = minus_partner;
      conn.src_outport = outport;
      conn.dst_inport = inport;
      ++cidx;
    }
    dim_stride *= dimensions_[i];
  }
  conns.resize(cidx);
}

}
}
