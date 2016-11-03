#include <sstmac/hardware/pisces/packet_allocator.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/router/routable.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/hardware/network/network_message.h>


ImplementFactory(sstmac::hw::packet_allocator);

namespace sstmac {
namespace hw {

class pisces_default_packet_allocator :
 public packet_allocator
{
 public:
  pisces_default_packet_allocator(sprockit::sim_parameters* params)
    : packet_allocator(params)
  {
  }

  virtual pisces_payload*
  new_packet(int bytes, uint64_t flow_id, bool is_tail,
             node_id toaddr, node_id fromaddr,
             serializable *msg) override {
    return new pisces_default_packet(msg, flow_id, bytes, is_tail,
                             toaddr, fromaddr);
  }
};
SpktRegister("pisces | default", packet_allocator, pisces_default_packet_allocator);


class pisces_delay_stats_packet_allocator :
 public packet_allocator
{
public:
 pisces_delay_stats_packet_allocator(sprockit::sim_parameters* params)
   : packet_allocator(params)
 {
 }

 virtual pisces_payload*
 new_packet(int bytes, uint64_t flow_id, bool is_tail,
            node_id toaddr, node_id fromaddr,
            serializable *msg) override {
   return new pisces_delay_stats_packet(msg, flow_id, bytes, is_tail,
                            toaddr, fromaddr);
 }
};
SpktRegister("delay_stats", packet_allocator, pisces_delay_stats_packet_allocator);

}
}

