#ifndef sstmac_hw_NETLINK_H
#define sstmac_hw_NETLINK_H

#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/common/node_address.h>
#include <sstmac/hardware/interconnect/interconnect.h>

namespace sstmac {
namespace hw {

class netlink :
  public connectable,
  public event_subcomponent
{
 public:
  virtual ~netlink(){}

  int
  node_port(int node_offset) const {
    return num_tiles_ + node_offset;
  }

  bool
  is_node_port(int port) const {
    return port >= num_tiles_;
  }

  int
  switch_port(int tile_offset) const {
    return tile_offset;
  }

 protected:
  netlink(sprockit::sim_parameters* params, node* parent);

  int conc_;
  int num_tiles_;
  netlink_id id_;

};
DeclareFactory1InitParam(netlink, node*)

}
}

#endif // NETLINK_H
