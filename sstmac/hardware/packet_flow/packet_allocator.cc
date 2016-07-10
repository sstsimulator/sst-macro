#include <sstmac/hardware/packet_flow/packet_allocator.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/router/routable.h>
#include <sprockit/sim_parameters.h>


ImplementFactory(sstmac::hw::packet_allocator);

namespace sstmac {
namespace hw {

class geometry_routable_packet_flow :
 public packet_flow_payload,
 public geometry_routable
{
  NotSerializable(geometry_routable_packet_flow)

  public:
   geometry_routable_packet_flow(
     message* parent,
     int num_bytes,
     long offset) :
    packet_flow_payload(parent, num_bytes, offset),
    geometry_routable(parent->toaddr(), parent->fromaddr())
  {
  }

  node_id
  toaddr() const {
   return geometry_routable::toaddr();
  }

  node_id
  fromaddr() const {
    return geometry_routable::fromaddr();
  }

  int
  next_port() const {
    return geometry_routable::port();
  }

  int
  next_vc() const {
    return geometry_routable::vc();
  }

};

class geometry_routable_packet_allocator :
 public packet_allocator
{
 public:
  virtual packet_flow_payload*
  new_packet(long bytes, long byte_offset, message *msg){
    return new geometry_routable_packet_flow(msg, bytes, byte_offset);
  }

  virtual void
  init_factory_params(sprockit::sim_parameters *params){}
};

SpktRegister("geometry_routable", packet_allocator, geometry_routable_packet_allocator);

}
}

