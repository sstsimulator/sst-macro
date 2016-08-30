#include "xpressring.h"

namespace sstmac {
namespace hw {

SpktRegister("xpress", topology, xpress_ring,
            "A ring topology with express cables that make large jumps");

void
xpress_ring::init_factory_params(sprockit::sim_parameters* params)
{
  structured_topology::init_factory_params(params);
  ring_size_ = params->get_int_param("xpress_ring_size");
  jump_size_ = params->get_int_param("xpress_jump_size");
}

long
xpress_ring::num_leaf_switches() const
{
  return ring_size_;
}

void
xpress_ring::connect_objects(internal_connectable_map& objects)
{
  for (int i=0; i < ring_size_; ++i) {
    connectable* center_obj = objects[switch_id(i)];

    switch_id up_idx((i + 1) % ring_size_);
    connectable* up_partner = objects[up_idx];
    center_obj->connect(up_port, connectable::network_link, up_partner);

    switch_id down_idx((i + ring_size_ - 1) % ring_size_);
    connectable* down_partner = objects[down_idx];
    center_obj->connect(down_port, connectable::network_link,
                                    down_partner);

    switch_id jump_up_idx((i + jump_size_) % ring_size_);
    connectable* jump_up_partner = objects[jump_up_idx];
    center_obj->connect(jump_up_port, connectable::network_link,
                                    jump_up_partner);

    switch_id jump_down_idx((i + ring_size_ - jump_size_) % ring_size_);
    connectable* jump_down_partner = objects[jump_down_idx];
    center_obj->connect(jump_down_port, connectable::network_link,
                                    jump_down_partner);
  }
}

void
xpress_ring::minimal_route_to_coords(
  const coordinates& src_coords,
  const coordinates& dest_coords,
  structured_routable::path& path) const
{
  int src_pos = src_coords[0];
  int dest_pos = dest_coords[0];

  //can route up or down
  int up_distance = abs(dest_pos - src_pos);
  int down_distance = abs(src_pos + ring_size_ - dest_pos);

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
  const coordinates& src_coords,
  const coordinates& dest_coords) const
{
  int src_pos = src_coords[0];
  int dest_pos = dest_coords[0];
  int up_distance = abs(dest_pos - src_pos);
  int down_distance = abs(src_pos + ring_size_ - dest_pos);

  int total_distance = std::max(up_distance, down_distance);
  return num_hops(total_distance);
}

int
xpress_ring::ndimensions() const
{
  return 1;
}

switch_id
xpress_ring::switch_number(const coordinates& coords) const
{
  return switch_id(coords[0]);
}

void
xpress_ring::productive_path(
  int dim,
  const coordinates& src,
  const coordinates& dst,
  structured_routable::path& path) const
{
  minimal_route_to_coords(src, dst, path);
}

void
xpress_ring::compute_switch_coords(switch_id swid, coordinates& coords) const
{
  coords[0] = int(swid);
}

int
xpress_ring::convert_to_port(int dim, int dir) const
{
  switch(dim) {
    case UP {
      switch(dir) {
        case step:
          return up_port;
        case jump:
          return jump_up_port;
      }
    }
    case DOWN {
      switch(dir) {
        case step:
          return down_port;
        case jump:
          return jump_down_port;
      }
    }
    default:
      break;
  }
  spkt_throw_printf(sprockit::value_error,
                   "Invalid dim=%d,dir=%d for xpress ring topology",
                   dim, dir);
  return -1;
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

