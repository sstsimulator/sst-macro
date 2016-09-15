#include <sstmac/hardware/common/packetizer.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::packetizer)

RegisterDebugSlot(packetizer);

#define pkt_debug(...) debug_printf(sprockit::dbg::packetizer, "packetizer " __VA_ARGS__)

namespace sstmac {
namespace hw {

packetizer::packetizer(sprockit::sim_parameters* params,
           event_scheduler* parent,
           packetizer_callback* handler) :
  notifier_(handler),
  event_subscheduler(parent)
{
  packet_size_ = params->get_optional_byte_length_param("mtu", 4096);
}

packetizer::~packetizer()
{
  //do not delete - notifiers are owned by the person that passes them in
  //if (notifier_) delete notifier_;
}

void
packetizer::start(int vn, message *msg)
{
  pkt_debug("starting on vn %d message %s", vn, msg->to_string().c_str());

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
packetizer::sendWhatYouCan(int vn)
{
  std::list<pending_send>& pending = pending_[vn];
  while (!pending.empty()){
    pending_send& next = pending.front();
    long num_bytes = std::min(next.bytes_left, long(packet_size_));

    if (!spaceToSend(vn, num_bytes*8)){
      pkt_debug("no space to send %d bytes on vn %d", num_bytes, vn);
      return;
    }

    pkt_debug("injecting %d bytes on vn %d", num_bytes, vn);
    inject(vn, num_bytes, next.offset, next.msg);

    next.offset += num_bytes;
    next.bytes_left -= num_bytes;
    if (next.bytes_left == 0)
      pending.pop_front();
  }
}

void
packetizer::bytesArrived(int vn, uint64_t unique_id, int bytes, message *parent)
{
  message* done = completion_queue_.recv(unique_id, bytes, parent);
  if (done){
    notifier_->notify(vn, done);
  }
}

void
packetizer::packetArrived(int vn, packet* pkt)
{
  bytesArrived(vn, pkt->flow_id(), pkt->byte_length(), pkt->orig());
  delete pkt;
}

#if SSTMAC_INTEGRATED_SST_CORE
SimpleNetworkPacketizer::SimpleNetworkPacketizer(sprockit::sim_parameters *params,
                                                 event_scheduler* parent,
                                                 packetizer_callback *handler) :
  packetizer(params, parent, handler)
{
  SST::Params& sst_params = *params->extra_data<SST::Params>();
  m_linkControl = (SST::Interfaces::SimpleNetwork*)parent->loadSubComponent(
                  sst_params.find_string("module"), parent, sst_params);

  SST::UnitAlgebra link_bw(sst_params.find_string("injection_bandwidth"));
  SST::UnitAlgebra injection_buffer_size(sst_params.find_string("injection_credits"));
  SST::UnitAlgebra big_buffer("1GB");
  m_linkControl->initialize(sst_params.find_string("rtrPortName","rtr"),
                              link_bw, 1, injection_buffer_size, big_buffer);

  m_recvNotifyFunctor = new SST::Interfaces::SimpleNetwork::Handler<SimpleNetworkPacketizer>
      (this,&SimpleNetworkPacketizer::recvNotify );

  m_sendNotifyFunctor = new SST::Interfaces::SimpleNetwork::Handler<SimpleNetworkPacketizer>
      (this,&SimpleNetworkPacketizer::sendNotify );

  m_linkControl->setNotifyOnReceive( m_recvNotifyFunctor );
  m_linkControl->setNotifyOnSend( m_sendNotifyFunctor );
}

bool
SimpleNetworkPacketizer::spaceToSend(int vn, int num_bits) const
{
  return m_linkControl->spaceToSend(vn, num_bits);
}

bool
SimpleNetworkPacketizer::sendNotify(int vn)
{
  sendWhatYouCan(vn);
  return true;
}

bool
SimpleNetworkPacketizer::recvNotify(int vn)
{
  SST::Interfaces::SimpleNetwork::Request* req = m_linkControl->recv(vn);
  message* m = 0;
  uint64_t flow_id;
  if (req->tail){
    m = static_cast<message*>(req->takePayload());
    flow_id = m->flow_id();
  } else {
    SimpleNetworkPacket* p = static_cast<SimpleNetworkPacket*>(req->takePayload());
    flow_id = p->flow_id;
    delete p;
  }
  bytesArrived(vn, flow_id, req->size_in_bits/8, m);
  delete req;
  return true;
}

void
SimpleNetworkPacketizer::inject(int vn, long bytes, long byte_offset, message* payload)
{
  SST::Interfaces::SimpleNetwork::nid_t dst = payload->toaddr();
  SST::Interfaces::SimpleNetwork::nid_t src = payload->toaddr();
  bool head = bytes == 0;
  bool tail = (bytes + byte_offset) == payload->byte_length();
  SST::Event* ev_payload;
  if (tail){
    ev_payload = payload;
  } else {
    ev_payload = new SimpleNetworkPacket(payload->flow_id());
  }
  SST::Interfaces::SimpleNetwork::Request* req =
        new SST::Interfaces::SimpleNetwork::Request(dst, src, bytes*8, head, tail, ev_payload);
  m_linkControl->send(req, vn);
}
#endif


}
}
