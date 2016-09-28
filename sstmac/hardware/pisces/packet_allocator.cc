#include <sstmac/hardware/pisces/packet_allocator.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/router/routable.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/hardware/network/network_message.h>


ImplementFactory(sstmac::hw::packet_allocator);

namespace sstmac {
namespace hw {

/**
 * @brief The pisces_packet class
 * The default packet flow class.  A geometry-routable packet
 * is compatible with the
 */
class pisces_packet :
 public pisces_payload,
 public routable
{
  NotSerializable(pisces_packet)

  public:
   pisces_packet(
     serializable* msg,
     uint64_t flow_id,
     int num_bytes,
     long offset,
     node_id toaddr,
     node_id fromaddr) :
    pisces_payload(msg, flow_id, num_bytes, offset),
    routable(toaddr, fromaddr)
  {
  }

  node_id
  toaddr() const override {
   return routable::toaddr();
  }

  node_id
  fromaddr() const override {
    return routable::fromaddr();
  }

  int
  next_port() const override {
    return routable::port();
  }

  int
  next_vc() const override {
    return routable::vc();
  }

};

class pisces_packet_allocator :
 public packet_allocator
{
 public:
  pisces_packet_allocator(sprockit::sim_parameters* params)
    : packet_allocator(params)
  {
  }

  virtual pisces_payload*
  new_packet(int bytes, long byte_offset,
             node_id toaddr, node_id fromaddr,
             uint64_t flow_id, serializable *msg) override {
    return new pisces_packet(msg, flow_id, bytes, byte_offset,
                             toaddr, fromaddr);
  }

};
SpktRegister("pisces", packet_allocator, pisces_packet_allocator);



}
}

