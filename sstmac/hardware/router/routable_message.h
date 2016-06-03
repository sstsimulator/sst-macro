#ifndef sstmac_hardware_network_interconnect_INTERCONNECT_MESSAGE_H
#define sstmac_hardware_network_interconnect_INTERCONNECT_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_info.h>

namespace sstmac {
namespace hw {

class routable
{

 public:
  node_id
  toaddr() const {
    return toaddr_;
  }

  node_id
  fromaddr() const {
    return fromaddr_;
  }

  void
  set_toaddr(node_id to) {
    toaddr_ = to;
  }

  void
  set_fromaddr(node_id from) {
    fromaddr_ = from;
  }

  const routing_info&
  rinfo() const {
    return rinfo_;
  }

  routing_info&
  rinfo() {
    return rinfo_;
  }

  int
  port() const {
    return rinfo_.port();
  }

  int
  vc() const {
    return rinfo_.vc();
  }

  routing::algorithm_t
  algo() const {
    return rinfo_.route_algo();
  }

  void
  set_algo(routing::algorithm_t algo) {
    rinfo_.set_route_algo(algo);
  }

  void
  serialize_order(serializer& ser);

  void
  add_hop() {
    ++n_hops_;
  }

  int
  n_hops() {
    return n_hops_;
  }

 protected:
  routable() : n_hops_(0) {}

  routable(node_id toaddr, node_id fromaddr);

 protected:
  routing_info rinfo_;

  node_id toaddr_;

  node_id fromaddr_;

  int n_hops_;

};


}
}

#endif // INTERCONNECT_MESSAGE_H

