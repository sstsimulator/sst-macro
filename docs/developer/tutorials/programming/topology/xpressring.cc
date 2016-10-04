#include "xpressring.h"

namespace sstmac {
namespace hw {

SpktRegister("xpress", topology, xpress_ring,
            "A ring topology with express cables that make large jumps");

xpress_ring::xpress_ring(sprockit::sim_parameters* params)
  : structured_topology(params, InitMaxPortsIntra::I_Remembered, InitGeomEjectID::I_Remembered)
{
  ring_size_ = params->get_int_param("xpress_ring_size");
  jump_size_ = params->get_int_param("xpress_jump_size");
  max_ports_intra_network_ = 4;
  eject_geometric_id_ = max_ports_intra_network_;
}

void
xpress_ring::connected_outports(switch_id src,
                            std::vector<connection>& conns) const
{
  conns.resize(4); //+1/-1 conns, +jump,-jump conns
  conns[0].src = src;
  conns[0].dst = (src+1) % ring_size_;
  conns[0].src_outport = up_port;
  conns[0].dst_inport = down_port;

  conns[1].src = src;
  conns[1].dst = (src+ring_size_ - 1) % ring_size_;
  conns[1].src_outport = down_port;
  conns[1].dst_inport = up_port;

  conns[2].src = src;
  conns[2].dst = (src+jump_size_) % ring_size_;
  conns[2].src_outport = jump_up_port;
  conns[2].dst_inport = jump_down_port;

  conns[3].src = src;
  conns[3].dst = (src-jump_size_+ring_size_) % ring_size_;
  conns[3].src_outport = jump_down_port;
  conns[3].dst_inport = jump_up_port;
}

void
xpress_ring::configure_individual_port_params(switch_id src, sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, 4, switch_params);
}

void
xpress_ring::configure_vc_routing(std::map<routing::algorithm_t, int>& m) const 
{
  m[routing::minimal] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

void
xpress_ring::minimal_route_to_switch(
  switch_id src,
  switch_id dest,
  routable::path& path) const
{
  //can route up or down
  int up_distance = abs(dest - src);
  int down_distance = abs(src + ring_size_ - dest);

  int xpress_cutoff = jump_size_ / 2;

  if (up_distance <= down_distance) {
    if (up_distance > xpress_cutoff) {
      path.outport = jump_up_port;
      path.vc = 0;
    }
    else {
      path.outport = up_port;
      path.vc = 0;
    }
  }
  else {
    if (down_distance > xpress_cutoff) {
      path.outport = jump_down_port;
      path.vc = 0;
    }
    else {
      path.outport = down_port;
      path.vc = 0;
    }
  }
}

int
xpress_ring::num_hops(int total_distance) const
{
  int num_jumps = total_distance / jump_size_;
  int num_steps = total_distance % jump_size_;
  int half_jump = jump_size_ / 2;
  if (num_steps > half_jump) {
    //take an extra jump
    ++num_jumps;
    num_steps = jump_size_ - num_steps;
  }
  return num_jumps + num_steps;
}

int
xpress_ring::minimal_distance(
  switch_id src,
  switch_id dest) const
{
  int up_distance = abs(dest - src);
  int down_distance = abs(src + ring_size_ - dest);

  int total_distance = std::max(up_distance, down_distance);
  return num_hops(total_distance);
}

int
xpress_ring::diameter() const
{
  //half-way around the ring is the biggest
  int halfway = ring_size_ / 2;
  return num_hops(halfway);
}



}
}

