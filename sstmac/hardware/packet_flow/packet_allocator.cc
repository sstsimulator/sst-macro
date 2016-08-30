#include <sstmac/hardware/packet_flow/packet_allocator.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/router/routable.h>
#include <sprockit/sim_parameters.h>


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
 public structured_routable
{
  NotSerializable(structured_routable_packet_flow)

  public:
   structured_routable_packet_flow(
     message* parent,
     int num_bytes,
     long offset) :
    packet_flow_payload(parent, num_bytes, offset),
    structured_routable(parent->toaddr(), parent->fromaddr())
  {
  }

  node_id
  toaddr() const {
   return structured_routable::toaddr();
  }

  node_id
  fromaddr() const {
    return structured_routable::fromaddr();
  }

  int
  next_port() const {
    return structured_routable::port();
  }

  int
  next_vc() const {
    return structured_routable::vc();
  }

};

class structured_routable_packet_allocator :
 public packet_allocator
{
 public:
  virtual packet_flow_payload*
  new_packet(int bytes, long byte_offset, message *msg){
    return new structured_routable_packet_flow(msg, bytes, byte_offset);
  }

  virtual void
  init_factory_params(sprockit::sim_parameters *params){}
};

SpktRegister("structured_routable", packet_allocator, structured_routable_packet_allocator);

}
}

