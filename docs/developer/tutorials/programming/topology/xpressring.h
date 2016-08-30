#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

class xpress_ring :
  public structured_topology
{
 public:
  typedef enum {
    up_port = 0,
    down_port = 1,
    jump_up_port = 2,
    jump_down_port = 3
  } port_t;

  typedef enum {
    jump = 0, step = 1
  } stride_t;

 public:
  virtual ~xpress_ring() {}

  virtual std::string
  to_string() const {
    return "xpress ring topology";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  /**
  * @brief Given a set of connectables, connect them appropriately
  * @param objects The set of objects to connect
  * @param cloner   If in parallel mode, not all objects may exist.
  *                 The factory creates missing objects.
  * @throws value_error If invalid switchid is passed to cloner
  */
  virtual void
  connect_objects(internal_connectable_map& objects);

  /**
  Structured topologies can be direct (torus) or indirect (fat tree).
  We therefore need to distinguish the total number of switches and
  the number of leaf switches - i.e. those directly connected to nodes.
  For direct topologies, num_switches and num_leaf_switches are the same.
  For indirect, num_leaf_switches < num_switches.
  @return The number of leaf switches directly connected to compute nodes
  */
  virtual long
  num_leaf_switches() const;

  /**
  Workhorse function for implementing #minimal_route_to_switch
  and #minimal_route_to_node.
  Given source/dest coordinates, find the minimal path.
  @param current_sw_addr The addr of the current switch
  @param dest_sw_addr The addr of the destination switch
  @param path [inout] A complete path descriptor to the destination switch
  */
  virtual void
  minimal_route_to_coords(
    const coordinates& src_coords,
    const coordinates& dest_coords,
    structured_routable::path& path) const;

  /**
  The function accepts either source or node coordinates.
  This gives the minimal distance counting the number of hops between switches.
  If node coordinates are given, the last coordinate is just ignored.
  @param src_coords. The source coordinates. This can be either switch or node coordinates.
  @param dest_coords. The destination coordinates. This can be either switch or node coordinates.
  @return The number of hops to final destination
  */
  virtual int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const;

  /**
  The number of distinct 'dimensions'
  in the topology.  This can correspond directly to standard
  X,Y,Z dimensions or the the number of levels in a fat tree.
  The keyword topology_redundant vector should have this many
  entries.
  */
  virtual int
  ndimensions() const;

  virtual switch_id
  switch_number(const coordinates& coords) const;

  /**
  @param dim The dimension you want to move in.
          You might need to traverse other dimensions FIRST
          before you make progress on this dimension.
  @param src The coordinates of the source switch
  @param dst The coordinates of the dest switch
  @param path [inout] The path configuration for making progress on dim
  */
  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    structured_routable::path& path) const;

  virtual int
  radix() const {
    return 4;
  }

  virtual long
  num_switches() const {
    return ring_size_;
  }

  int
  diameter() const;

  virtual int
  convert_to_port(int dim, int dir) const;

 protected:
  /**
  Compute coordinates (e.g. X,Y,Z for 3D torus)
  @param swid The unique index defining a switch location
  @param coords [inout] The unique coordinates of the switch
  */
  virtual void
  compute_switch_coords(switch_id swid, coordinates& coords) const;

  int
  num_hops(int total_distance) const;

 protected:
  int ring_size_;

  int jump_size_;

};

}
}

