#ifndef PACKET_ALLOCATOR_H
#define PACKET_ALLOCATOR_H

#include <sprockit/factories/factory.h>
#include <sstmac/hardware/pisces/pisces_fwd.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/common/serializable.h>
#include <sstmac/common/node_address.h>

namespace sstmac {
namespace hw {

/**
 * @brief The packet_allocator class
 * Factory for creating packets.
 * Default packet allocator adds the bare minimum needed for congestion modeling.
 * Non-default packet allocators can add extra fields to the packets to track more statistics.
 */
class packet_allocator
{
 public:
  packet_allocator(sprockit::sim_parameters* params){}

  /**
   * @brief new_packet Allocates a new packet corresponding to a subset
   *  of a flow (message)
   * @param bytes       Number of bytes in the packet
   * @param byte_offset The offset within the message (flow) the packet begins at
   * @param msg         The message being packetized
   * @return  A packet compatible with pisces model
   */
  virtual pisces_payload*
  new_packet(int bytes, uint64_t flow_id, bool is_tail,
             node_id toaddr, node_id fromaddr,
             serializable* msg) = 0;

  virtual ~packet_allocator(){}

};

DeclareFactory(packet_allocator)

}
}

#endif // PACKET_ALLOCATOR_H
