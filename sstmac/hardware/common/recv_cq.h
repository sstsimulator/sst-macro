#ifndef NIC_RECV_CQ_H
#define NIC_RECV_CQ_H

#include <sprockit/unordered.h>
#include <sstmac/hardware/common/packet.h>

namespace sstmac {
namespace hw {
/**
A Receive Completion Queue.
Based on packets coming in, it determines when a message has completely arrived.
A large message, in some models, is broken up into many #message_chunk
objects.  When using minimal, in-order routing, tracking message completion is easier
because a packet can be marked as the "tail" and used to track when an entire message has arrived.
When using adaptive or multipath routing, messages arrive out-of-order.
This class tracks whether all packets have been received and signals to some handler
that an entire message has fully arrived.
@class nic_recv_cq
*/
class recv_cq
{

 public:
  /**
      Log packet and determine if parent message has fully arrived
      @param packet The arriving packet
      @return The completed msg or a null msg indicating not yet complete
  */
  message*
  recv(packet* pkt);

  void
  print();

 protected:
  struct incoming_msg {
    message* msg;
    long bytes_arrived;
    long bytes_total;
    incoming_msg() :
        bytes_arrived(0),
        bytes_total(0),
        msg(0)
    {
    }
  };

  /**
      Keys are unique network ID for all messages.
      Value is the number of bytes receved.
  */
  typedef spkt_unordered_map<uint64_t, incoming_msg> received_map;
  received_map bytes_recved_;
};

}
}


#endif // NIC_RECV_CQ_H

