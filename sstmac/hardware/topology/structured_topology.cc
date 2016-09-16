#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/common/sim_partition.h>

namespace sstmac {
namespace hw {


structured_topology::structured_topology(sprockit::sim_parameters* params,
                                         InitMaxPortsIntra i1,
                                         InitGeomEjectID i2) :
  topology(params,i1,i2)
{
}

coordinates
structured_topology::switch_coords(switch_id uid) const
{
  coordinates coords(ndimensions());
  compute_switch_coords(uid, coords);
  return coords;
}

coordinates
structured_topology::endpoint_coords(node_id nid) const
{
  int nps = endpoints_per_switch_;
  if (nps == 1) {
    coordinates coords(ndimensions());
    switch_id swid((int)nid);
    compute_switch_coords(swid, coords);
    return coords;
  }
  else {
    switch_id swid(nid / nps);
    int lastidx = nid % nps;
    //extra one for switch coordinates
    int ndim = ndimensions();
    coordinates coords(ndim + 1);
    compute_switch_coords(swid, coords);
    coords[ndim] = lastidx;
    return coords;
  }
}

coordinates
structured_topology::node_coords(node_id uid) const
{
  if (concentration_ == 1) {
    coordinates coords(ndimensions());
    switch_id swid((long) uid);
    compute_switch_coords(swid, coords);
    return coords;
  }
  else {
    switch_id swid(uid / concentration_);
    int lastidx = uid % concentration_;
    //extra one for switch coordinates
    int ndim = ndimensions();
    coordinates coords(ndim + 1);
    compute_switch_coords(swid, coords);
    coords[ndim] = lastidx;
    return coords;
  }
}

node_id
structured_topology::node_addr(const coordinates &sw_coords, int port) const
{
  long net_id = switch_number(sw_coords);
  if (net_id >= num_leaf_switches()) {
    spkt_throw_printf(sprockit::value_error,
                     "structured_topology::node_addr: invalid switch id %ld. check coordinates",
                     net_id);
  }
  long nid = endpoints_per_switch_ * net_id + port;
  return node_id(nid);
}

node_id
structured_topology::node_addr(const coordinates& coords) const
{

  if (coords.size() == ndimensions()) {
    //there is no "extra" switch index
    long swid = switch_number(coords);
    return node_id(swid);
  }

  int lastidx = coords.size() - 1;
  return node_addr(coords, coords[lastidx]);
}

switch_id
structured_topology::endpoint_to_injection_switch(
    node_id nodeaddr, int& switch_port) const
{
  int sw_id = nodeaddr / endpoints_per_switch_;
  /** Ejection/injection ports begin at radix_ */
  switch_port = nodeaddr % endpoints_per_switch_ + max_ports_intra_network_;
  return switch_id(sw_id);
}

void
structured_topology::eject_paths_on_switch(
   node_id dest_addr, switch_id sw_addr, structured_routable::path_set &paths) const
{
  int node_offset = dest_addr % endpoints_per_switch_;
  int switch_port = node_offset + max_ports_intra_network_;
  paths.resize(1);
  paths[0].outport = switch_port;
  paths[0].vc = 0;
  paths[0].geometric_id = eject_geometric_id_ + node_offset;
}

void
structured_topology::configure_injection_geometry(std::vector<int>& redundancies)
{
  for (int i=0; i < endpoints_per_switch_; ++i){
    redundancies[i+eject_geometric_id_] = injection_redundancy_;
  }
}

int
structured_topology::endpoint_to_injection_port(node_id nodeaddr) const
{
  int net_id = nodeaddr / endpoints_per_switch_;
  /** Ejection/injection ports begin at radix_ */
  return net_id % endpoints_per_switch_ + max_ports_intra_network_;
}

switch_id
structured_topology::endpoint_to_ejection_switch(node_id nodeaddr,
    int& switch_port) const
{
  return structured_topology::endpoint_to_injection_switch(nodeaddr, switch_port);
}

int
structured_topology::endpoint_to_ejection_port(node_id addr) const
{
  int node_id = addr;
  /** Ejection/injection ports begin at radix_ */
  return node_id % endpoints_per_switch_ + max_ports_intra_network_;
}

void
structured_topology::productive_paths(
  structured_routable::path_set &paths,
  const coordinates &current,
  const coordinates &dst)
{
  //first count the number of productive paths
  int ncoords = current.size();
  int npaths = 0;
  for (int i=0; i < ncoords; ++i) {
    npaths += current[i] == dst[i] ? 0 : 1;
  }
  paths.resize(npaths);

  int pathidx = 0;
  for (int i=0; i < ncoords; ++i) {
    if (current[i] != dst[i]) {
      structured_routable::path& next_path = paths[pathidx++];
      productive_path(i, current, dst, next_path);
    }
  }
}

void
structured_topology::minimal_route_to_switch(
  switch_id current_sw_addr,
  switch_id dest_sw_addr,
  structured_routable::path& path) const
{
  coordinates src_coords = switch_coords(current_sw_addr);
  coordinates dest_coords = switch_coords(dest_sw_addr);
  minimal_route_to_coords(src_coords, dest_coords, path);
}

coordinates
structured_topology::neighbor_at_port(switch_id sid, int port)
{
  spkt_throw(sprockit::unimplemented_error,
    "topology::neighbor_at_port");
}

int
structured_topology::num_hops_to_node(node_id src, node_id dst) const
{

  int ret = minimal_distance(
              node_coords(src),
              node_coords(dst));
  return ret;
}

void
structured_topology::bit_complement_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  std::string topname = to_string();
  spkt_throw_printf(sprockit::unimplemented_error,
                   "%s::get_bit_complement_partners: "
                   "don't know how for %s",
                   topname.c_str(), topname.c_str());
}

void
structured_topology::tornado_send_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  std::string topname = to_string();
  spkt_throw_printf(sprockit::unimplemented_error,
                   "%s::get_tornado_partners: "
                   "don't know how for %s",
                   topname.c_str(), topname.c_str());
}

void
structured_topology::tornado_recv_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  std::string topname = to_string();
  spkt_throw_printf(sprockit::unimplemented_error,
                   "%s::get_tornado_partners: "
                   "don't know how for %s",
                   topname.c_str(), topname.c_str());
}

void
structured_topology::nearest_neighbor_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  std::string topname = to_string();
  spkt_throw_printf(sprockit::unimplemented_error,
                   "%s::get_nearest_neighbor_partners: "
                   "don't know how for %s",
                   topname.c_str(), topname.c_str());
}

void
structured_topology::send_partners(
  traffic_pattern::type_t ty,
  node_id src,
  std::vector<node_id>& partner_list) const
{
  bool is_send = true;
  partners(is_send, ty, src, partner_list);
}

void
structured_topology::recv_partners(
  traffic_pattern::type_t ty,
  node_id src,
  std::vector<node_id>& partner_list) const
{
  bool is_send = false;
  partners(is_send, ty, src, partner_list);
}

void
structured_topology::partners(
  bool get_send_partner,
  traffic_pattern::type_t ty,
  node_id src,
  std::vector<node_id>& partners) const
{
  // convert to switch coordinates
  int ignore;
  switch_id src_sw = endpoint_to_injection_switch(src, ignore);
  int offset = int(src) % endpoints_per_switch_;
  coordinates src_sw_coords = switch_coords(src_sw);
  switch(ty) {
    case traffic_pattern::nearest_neighbor:
      nearest_neighbor_partners(src_sw_coords, offset, partners);
      break;
    case traffic_pattern::tornado:
      if (get_send_partner) {
        tornado_send_partners(src_sw_coords, offset, partners);
      }
      else {
        tornado_recv_partners(src_sw_coords, offset, partners);
      }
      break;
    case traffic_pattern::bit_complement:
      bit_complement_partners(src_sw_coords, offset, partners);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
                       "unknown traffic pattern enum %d",
                       ty);
  }
}

std::string
structured_topology::label(node_id nid) const
{
  return node_coords(nid).to_string();
}

std::string
structured_topology::label(switch_id sid) const
{
  return switch_coords(sid).to_string();
}

}
}


