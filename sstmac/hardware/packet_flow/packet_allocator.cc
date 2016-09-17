#include <sstmac/hardware/packet_flow/packet_allocator.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/router/routable.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/hardware/network/network_message.h>


ImplementFactory(sstmac::hw::packet_allocator);

namespace sstmac {
namespace hw {

/**
 * @brief The structured_routable_packet_flow class
 * The default packet flow class.  A geometry-routable packet
 * is compatible with the
 */
class structured_routable_packet_flow :
 public packet_flow_payload,
 public routable
{
  NotSerializable(structured_routable_packet_flow)

  public:
   structured_routable_packet_flow(
     message* parent,
     int num_bytes,
     long offset) :
    packet_flow_payload(parent, num_bytes, offset),
    routable(parent->toaddr(), parent->fromaddr())
  {
  }

  node_id
  toaddr() const {
   return routable::toaddr();
  }

  node_id
  fromaddr() const {
    return routable::fromaddr();
  }

  int
  next_port() const {
    return routable::port();
  }

  int
  next_vc() const {
    return routable::vc();
  }

 private:
};

class structured_routable_packet_allocator :
 public packet_allocator
{
 public:
  structured_routable_packet_allocator(sprockit::sim_parameters* params)
    : packet_allocator(params)
  {
  }

  virtual packet_flow_payload*
  new_packet(int bytes, long byte_offset, message *msg){
    return new structured_routable_packet_flow(msg, bytes, byte_offset);
  }

};




SpktRegister("structured_routable", packet_allocator, structured_routable_packet_allocator);

}
}

