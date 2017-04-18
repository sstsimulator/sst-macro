
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

  max_ports_injection_ =
      netlinks_per_switch_ * injection_redundancy_;

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
    routable::path_set &paths,
    switch_id addr) const
{
  if (current.outport() < first_simple_torus_eject_port_){
    //intranetwork routing
    int dim = current.outport() / 2; //2 for +/-
    int dir = current.outport() % 2;
    int red = red_[dim];
    paths.resize(red);
    int port_offset = tile_offsets_[dim] + red * dir;

    //outport identifies a unique path
    for (int r=0; r < red; ++r){
      paths[r] = current;
      paths[r].geometric_id = current.outport();
      paths[r].set_outport(port_offset + r);
    }
  } else {
    //ejection routing
    int offset = current.outport() - first_simple_torus_eject_port_;
    int port = max_ports_intra_network_ + offset*injection_redundancy_;
    int num_ports = injection_redundancy_;
    for (int i=0; i < num_ports; ++i, ++port){
      paths[i].set_outport(port);
    }
  }
}

void
tiled_torus::configure_individual_port_params(switch_id src,
                                       sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, max_ports_intra_network_, switch_params);
}

void
tiled_torus::configure_geometric_paths(std::vector<int>& redundancies)
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

  for (int i=0; i < ngeom_paths; ++i )
    std::cerr << sprockit::printf("redundancies[%i]: %i\n", i, redundancies[i]);
}

void
tiled_torus::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  std::cerr << "getting connected ouports for switch " << src << "\n";
  int max_port = max_ports_intra_network_ - 1;
  std::cerr << "max_port: " << max_port << "\n";
  conns.resize(max_port);
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
    std::cerr << "nreplica: " << red_[i] << std::endl;
    int outport, inport;
    for (int r=0; r < nreplica; ++r){
      if ( (cidx + 1) > max_port ) {
        std::cerr << "overrunning\n";
        abort();
      }
      outport = inport = port(r, i, hdtorus::pos);
      std::cerr << sprockit::printf("setting pos connection %i to node/port %i/%i\n", cidx, plus_partner, outport);
      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = plus_partner;
      conn.src_outport = outport;
      conn.dst_inport = inport;
      ++cidx;

      outport = inport = port(r, i, hdtorus::neg);
      std::cerr << sprockit::printf("setting neg connection %i to node/port %i/%i\n", cidx, minus_partner, outport);
      conn = conns[cidx];
      conn.src = src;
      conn.dst = minus_partner;
      conn.src_outport = outport;
      conn.dst_inport = inport;
      ++cidx;
    }
    dim_stride *= dimensions_[i];
  }
  //conns.resize(cidx);
}

//switch_id
//tiled_torus::netlink_to_injection_switch(node_id nodeaddr, uint16_t ports[], int &num_ports) const
//{
//  uint16_t port;
//  switch_id sid = hdtorus::netlink_to_injection_switch(nodeaddr, port);
//  num_ports = injection_redundancy_;
//  for (int i=0; i < num_ports; ++i, ++port){
//    ports[i] = port;
//  }
//  return sid;
//}

switch_id
tiled_torus::netlink_to_injection_switch(
    node_id netladdr, uint16_t ports[], int &num_ports) const
{
  // UGLY: interconnect calls this, it needs to return a real port number
  num_ports = injection_redundancy_;
  long sw = netladdr / netlinks_per_switch_;
  int local_endpoint_id = netladdr % netlinks_per_switch_;
  int first_port =
      max_ports_intra_network_ + local_endpoint_id * injection_redundancy_;
  for (int i=0; i < injection_redundancy_; ++i)
    ports[i] = first_port + i;
  return switch_id(sw);
}

switch_id
tiled_torus::netlink_to_ejection_switch(
    node_id nodeaddr, uint16_t ports[], int &num_ports) const
{
  return netlink_to_injection_switch(nodeaddr,ports,num_ports);
}

switch_id
tiled_torus::netlink_to_injection_switch(netlink_id netladdr, uint16_t& switch_port) const {
  // UGLY: route calls this, it needs to return a geometric id for port
  long sw = netladdr / netlinks_per_switch_;
  int local_endpoint_id = netladdr % netlinks_per_switch_;
  switch_port = first_simple_torus_eject_port_ + local_endpoint_id;
  return switch_id(sw);
}

switch_id
tiled_torus::netlink_to_ejection_switch(node_id nodeaddr, uint16_t& switch_port) const {
  return netlink_to_injection_switch(nodeaddr, switch_port);
}

}
}
