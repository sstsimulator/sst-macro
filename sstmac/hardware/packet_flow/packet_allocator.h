#ifndef PACKET_ALLOCATOR_H
#define PACKET_ALLOCATOR_H

#include <sprockit/factories/factory.h>
#include <sstmac/hardware/packet_flow/packet_flow_fwd.h>
#include <sstmac/common/messages/sst_message_fwd.h>

namespace sstmac {
namespace hw {

class packet_allocator : public sprockit::factory_type
{
 public:
  virtual packet_flow_payload*
  new_packet(int bytes, long byte_offset, message* msg) = 0;
};

DeclareFactory(packet_allocator)

}
}

#endif // PACKET_ALLOCATOR_H
