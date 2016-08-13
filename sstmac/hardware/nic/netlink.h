#ifndef sstmac_hw_NETLINK_H
#define sstmac_hw_NETLINK_H

#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/common/node_address.h>

namespace sstmac {
namespace hw {

class netlink :
  public sprockit::factory_type,
  public connectable,
  public event_subscheduler
{
 public:
  virtual ~netlink(){}
  
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  int
  node_port(int node_offset) const {
    return num_inject_ + node_offset;
  }

  int
  switch_port(int tile_offset) const {
    return tile_offset;
  }

 protected:
  netlink(sprockit::factory_type* ty){}

  int num_eject_;
  int num_inject_;
  netlink_id id_;

};
DeclareFactory1InitParam(netlink, sprockit::factory_type*/*interconnect*/)

}
}

#endif // NETLINK_H
