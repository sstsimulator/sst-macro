#ifndef PACKET_ALLOCATOR_H
#define PACKET_ALLOCATOR_H

#include <sprockit/factories/factory.h>
#include <sstmac/hardware/packet_flow/packet_flow_fwd.h>
#include <sstmac/common/messages/sst_message_fwd.h>

namespace sstmac {
namespace hw {

/**
 * @brief The packet_allocator class
 * Factory for creating packets.
 * Default packet allocator adds the bare minimum needed for congestion modeling.
 * Non-default packet allocators can add extra fields to the packets to track more statistics.
 */
class packet_allocator : public sprockit::factory_type
{
 public:
  /**
   * @brief new_packet Allocates a new packet corresponding to a subset
   *  of a flow (message)
   * @param bytes       Number of bytes in the packet
   * @param byte_offset The offset within the message (flow) the packet begins at
   * @param msg         The message being packetized
   * @return  A packet compatible with packet_flow model
   */
  virtual packet_flow_payload*
  new_packet(int bytes, long byte_offset, message* msg) = 0;

  virtual ~packet_allocator(){}

};

DeclareFactory(packet_allocator)

}
}

#endif // PACKET_ALLOCATOR_H
