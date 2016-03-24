#include <sstmac/hardware/common/packetizer.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::packetizer)

namespace sstmac {
namespace hw {

void
packetizer::start(int vn, message *msg)
{
  pending_send next;
  next.bytes_left = msg->byte_length();
  if (next.bytes_left == 0){
    spkt_throw_printf(sprockit::value_error,
        "packet_flow_injection_buffer::start: starting message with zero length: %s",
        msg->to_string().c_str());
  }
  next.offset = 0;
  next.msg = msg;
  pending_[vn].push_back(next);

  sendWhatYouCan(vn);
}

void
packetizer::init_factory_params(sprockit::sim_parameters *params)
{
  packet_size_ = params->get_optional_byte_length_param("packet_size", 4096);
}

void
packetizer::sendWhatYouCan(int vn)
{
  std::list<pending_send>& pending = pending_[vn];
  while (!pending.empty()){
    pending_send& next = pending.front();
    long num_bytes = std::min(next.bytes_left, long(packet_size_));

    if (!spaceToSend(vn, num_bytes*8)){
      return;
    }

    inject(vn, num_bytes, next.offset, next.msg);

    next.offset += num_bytes;
    next.bytes_left -= num_bytes;
    if (next.bytes_left == 0)
      pending.pop_front();
  }
}

void
packetizer::packetArrived(int vn, packet* pkt)
{
  message* parent_msg = completion_queue_.recv(pkt);
  if (parent_msg){
    notifier_->notify(vn, parent_msg);
  }
  delete pkt;
}

}
}
