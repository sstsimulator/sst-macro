#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/recv_cq.h>
#include <sstmac/common/messages/sst_message.h>
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

message*
recv_cq::recv(uint64_t unique_id, int bytes, message* orig)
{
  incoming_msg& incoming  = bytes_recved_[unique_id];
#if SSTMAC_SANITY_CHECK
  if (incoming.msg && orig){
    spkt_throw_printf(sprockit::illformed_error,
        "recv_cq::recv: only one message chunk should carry the parent payload for %lu",
        unique_id);
  }
#endif
  if (orig){
    //this guy is actually carrying the payload
    incoming.msg = orig;
    incoming.bytes_total = orig->byte_length();
  }
  incoming.bytes_arrived += bytes;

#if SSTMAC_SANITY_CHECK
  if (incoming.msg && (incoming.bytes_arrived > incoming.bytes_total)){
    spkt_throw(sprockit::illformed_error,
        "recv_cq::recv: have too many bytes in queue "
        "for parent message ", incoming.msg->to_string(),
        " and packet ", incoming.msg->to_string());
  }
#endif
  if (incoming.bytes_arrived == incoming.bytes_total){
    message* ret = incoming.msg;
    bytes_recved_.erase(unique_id);
    return ret;
  }
  else {
    return NULL;
  }
}

message*
recv_cq::recv(packet* pkt)
{
  message* flow = dynamic_cast<message*>(pkt->orig());
  return recv(pkt->flow_id(), pkt->byte_length(), flow);
}

}
}

