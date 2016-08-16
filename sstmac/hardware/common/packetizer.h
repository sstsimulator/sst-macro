#ifndef PACKETIZER_H
#define PACKETIZER_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/common/recv_cq.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/integrated_component.h>
#include <sst/core/interfaces/simpleNetwork.h>
#endif

namespace sstmac {
namespace hw {

class packetizer_callback
{
 public:
  virtual void notify(int vn, message* msg) = 0;

  virtual ~packetizer_callback(){}
};

class packetizer :
  public sprockit::factory_type,
  public event_subscheduler
{

 public:
  virtual ~packetizer();

  void start(int vn, message* payload);

  void packetArrived(int vn, packet* pkt);

  void sendWhatYouCan(int vn);

  void setNotify(packetizer_callback* handler){
    notifier_ = handler;
  }

  int packetSize() const {
    return packet_size_;
  }

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  virtual void
  init_sst_params(SST::Params& params, SST::Component* parent){}
#endif

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

 private:
  virtual void inject(int vn, long bytes, long byte_offset, message* payload) = 0;

  virtual bool spaceToSend(int vn, int num_bits) const = 0;

 private:
  recv_cq completion_queue_;

  struct pending_send{
    message* msg;
    long bytes_left;
    long offset;
  };

  std::map<int, std::list<pending_send> > pending_;

  int packet_size_;

  packetizer_callback* notifier_;

 protected:
  packetizer() : notifier_(nullptr){}

  void bytesArrived(int vn, uint64_t unique_id, int bytes, message* parent);

};

DeclareFactory(packetizer)

#if SSTMAC_INTEGRATED_SST_CORE
class SimpleNetworkPacket : public SST::Event
{
  NotSerializable(SimpleNetworkPacket)

 public:
  SimpleNetworkPacket(uint64_t id) : unique_id(id) {}
  uint64_t unique_id;
};

class SimpleNetworkPacketizer :
  public packetizer
{
 public:
  bool spaceToSend(int vn, int num_bits) const;

  void inject(int vn, long bytes, long byte_offset, message *payload);

  virtual void init_sst_params(SST::Params &params, SST::Component *parent);

  bool recvNotify(int vn);

  bool sendNotify(int vn);

 private:
  SST::Interfaces::SimpleNetwork* m_linkControl;
  SST::Interfaces::SimpleNetwork::HandlerBase* m_recvNotifyFunctor;
  SST::Interfaces::SimpleNetwork::HandlerBase* m_sendNotifyFunctor;

};
#endif

}
}

#endif // PACKETIZER_H
