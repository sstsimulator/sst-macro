#include <sstmac/hardware/common/packetizer.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::hw::packetizer)

RegisterDebugSlot(packetizer);

#define pkt_debug(...) debug_printf(sprockit::dbg::packetizer, "packetizer " __VA_ARGS__)

namespace sstmac {
namespace hw {

packetizer::packetizer(sprockit::sim_parameters* params, event_scheduler* parent) :
  event_subcomponent(parent), //no self events
  notifier_(nullptr)
{
  packet_size_ = params->get_byte_length_param("mtu");
  double bw = params->get_bandwidth_param("bandwidth");
  inv_bw_ = 1.0 / bw;
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
        "pisces_injection_buffer::start: starting message with zero length: %s",
        msg->to_string().c_str());
  }
  next.offset = 0;
  next.msg = msg;
  pending_[vn].push_back(next);

  sendWhatYouCan(vn);
}

void
packetizer::deadlock_check()
{
  for (auto& pair : pending_){
    for (pending_send& send : pair.second){
      std::cerr << "Packetizer can't send " << send.msg->to_string() << std::endl;
    }
  }
}

void
packetizer::sendWhatYouCan(int vn)
{
  std::list<pending_send>& pending = pending_[vn];
  while (!pending.empty()){
    pending_send& next = pending.front();
    long initial_offset = next.offset;
    while (next.bytes_left){
      long num_bytes = std::min(next.bytes_left, long(packet_size_));
      if (!spaceToSend(vn, num_bytes*8)){
        pkt_debug("no space to send %d bytes on vn %d", num_bytes, vn);
        return;
      }
      pkt_debug("injecting %d bytes on vn %d", num_bytes, vn);
      inject(vn, num_bytes, next.offset, next.msg);

      next.offset += num_bytes;
      next.bytes_left -= num_bytes;
    }
    long bytes_sent = next.offset - initial_offset;
    if (next.msg->needs_ack()){
      timestamp time_to_send(bytes_sent * inv_bw_);
      schedule_delay(time_to_send, acker_, next.msg->clone_ack());
    }
    //the entire packet sent
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
  message* payload = dynamic_cast<message*>(pkt->orig());
  bytesArrived(vn, pkt->flow_id(), pkt->byte_length(), payload);
  delete pkt;
}

#if SSTMAC_INTEGRATED_SST_CORE
class SimpleNetworkPacket : public SST::Event
{
  NotSerializable(SimpleNetworkPacket)

 public:
  SimpleNetworkPacket(uint64_t id) : flow_id(id) {}
  uint64_t flow_id;
};

class merlin_packetizer :
  public packetizer
{
 public:
  merlin_packetizer(sprockit::sim_parameters* params,
                      event_scheduler* parent);

  std::string
  to_string() const {
    return "merling packetizer";
  }

  bool spaceToSend(int vn, int num_bits){
    return m_linkControl->spaceToSend(vn, num_bits);
  }

  void inject(int vn, long bytes, long byte_offset, message *payload);

  bool recvNotify(int vn);

  bool sendNotify(int vn){
    sendWhatYouCan(vn);
    return true;
  }

  void init(unsigned int phase){
    m_linkControl->init(phase);
  }

  void setup(){
    m_linkControl->setup();
  }

  link_handler*
  new_credit_handler() const{
    spkt_abort_printf("merlin_packetizier::new_ack_handler: never used");
  }

  link_handler*
  new_payload_handler() const{
    spkt_abort_printf("merlin_packetizier::new_payload_handler: never used");
  }

 private:
  SST::Interfaces::SimpleNetwork* m_linkControl;
  SST::Interfaces::SimpleNetwork::HandlerBase* m_recvNotifyFunctor;
  SST::Interfaces::SimpleNetwork::HandlerBase* m_sendNotifyFunctor;

};

merlin_packetizer::merlin_packetizer(sprockit::sim_parameters *params,
                                     event_scheduler* parent) :
  packetizer(params, parent)
{
  SST::Component* comp = safe_cast(SST::Component, parent);
  SST::Params& sst_params = *params->extra_data<SST::Params>();
  m_linkControl = (SST::Interfaces::SimpleNetwork*)comp->loadSubComponent(
                  params->get_optional_param("linkControl", "merlin.linkcontrol"),
                  comp, sst_params);

  SST::UnitAlgebra link_bw(params->get_param("bandwidth"));
  SST::UnitAlgebra injection_buffer_size(params->get_param("credits"));

  SST::UnitAlgebra big_buffer("1GB");
  m_linkControl->initialize(params->get_optional_param("rtrPortName","rtr"),
                            link_bw, 1, big_buffer, injection_buffer_size);

  m_recvNotifyFunctor = new SST::Interfaces::SimpleNetwork::Handler<merlin_packetizer>
      (this,&merlin_packetizer::recvNotify );

  m_sendNotifyFunctor = new SST::Interfaces::SimpleNetwork::Handler<merlin_packetizer>
      (this,&merlin_packetizer::sendNotify );

  m_linkControl->setNotifyOnReceive( m_recvNotifyFunctor );
  m_linkControl->setNotifyOnSend( m_sendNotifyFunctor );
}

bool
merlin_packetizer::recvNotify(int vn)
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
merlin_packetizer::inject(int vn, long bytes, long byte_offset, message* payload)
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
  pkt_debug("merling injecting %d bytes at offset %d on vn %d for message %s",
            bytes, byte_offset, vn, payload->to_string().c_str());
  m_linkControl->send(req, vn);
}

SpktRegister("merlin", packetizer, merlin_packetizer);
#endif


}
}
