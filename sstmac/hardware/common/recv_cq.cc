#include <sstmac/common/messages/message_chunk.h>
#include <sstmac/hardware/common/recv_cq.h>
#include <sprockit/output.h>

namespace sstmac {
namespace hw {

#define DEBUG_CQ 0

void
recv_cq::print()
{
  spkt_unordered_map<uint64_t,incoming_msg>::iterator it, end = bytes_recved_.end();
  coutn << "Completion Queue" << std::endl;
  for (it=bytes_recved_.begin(); it != end; ++it){
    incoming_msg& incoming = it->second;
    coutn << "Message " << it->first << " has "
        << incoming.bytes_arrived << " bytes arrived "
        << " out of " << incoming.bytes_total << "\n";
  }
}

sst_message::ptr
recv_cq::recv(const message_chunk::ptr& packet)
{
  incoming_msg& incoming  = bytes_recved_[packet->unique_id()];
  incoming.bytes_arrived += packet->byte_length();

#if SSTMAC_SANITY_CHECK
  if (incoming.msg && packet->orig()){
    spkt_throw(sprockit::illformed_error,
        "recv_cq::recv: only one message chunk should carry the parent payload");
  }
#endif

  if (packet->orig()){
#if DEBUG_CQ
    coutn << sprockit::printf("Got parent message for id %lu\n", packet->unique_id());
#endif
    //this guy is actually carrying the payload
    incoming.msg = packet->orig();
    incoming.bytes_total = packet->orig()->byte_length();
  }

#if DEBUG_CQ
    coutn << sprockit::printf("Now have %ld bytes for message %lu\n",
            incoming.bytes_arrived, packet->unique_id());
#endif

#if SSTMAC_SANITY_CHECK
  if (incoming.msg && (incoming.bytes_arrived > incoming.bytes_total)){
    spkt_throw(sprockit::illformed_error,
        "recv_cq::recv: have too many bytes in queue "
        "for parent message ", incoming.msg->to_string(),
        " and packet ", incoming.msg->to_string());
  }
#endif
  if (incoming.bytes_arrived == incoming.bytes_total){
#if DEBUG_CQ
    coutn << sprockit::printf("Ejecting id %lu\n", packet->unique_id());
#endif
    sst_message::ptr ret = incoming.msg;
    bytes_recved_.erase(packet->unique_id());
    return ret;
  }
  else {
    return sst_message::ptr();
  }
}

}
}

