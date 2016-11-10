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

DeclareDebugSlot(packetizer)

namespace sstmac {
namespace hw {

class packetizer_callback
{
 public:
  virtual void notify(int vn, message* msg) = 0;

  virtual ~packetizer_callback(){}
};

class packetizer :
  public event_subcomponent
{

 public:
  virtual ~packetizer();

  void start(int vn, message* payload);

  void packetArrived(int vn, packet* pkt);

  void sendWhatYouCan(int vn);

  void setArrivalNotify(packetizer_callback* handler){
    notifier_ = handler;
  }

  void deadlock_check();

  void setInjectionAcker(event_handler* handler){
    acker_ = handler;
  }

  int packetSize() const {
    return packet_size_;
  }

  virtual link_handler* new_payload_handler() const = 0;
  virtual link_handler* new_credit_handler() const = 0;

 private:
  virtual void
  inject(int vn, long bytes, long byte_offset, message* payload) = 0;

  virtual bool
  spaceToSend(int vn, int num_bits) = 0;

 private:
  recv_cq completion_queue_;

  struct pending_send{
    message* msg;
    long bytes_left;
    long offset;
  };

  std::map<int, std::list<pending_send> > pending_;

  int packet_size_;

  double inv_bw_;

  packetizer_callback* notifier_;
  event_handler* acker_;

 protected:
  packetizer(sprockit::sim_parameters* params,
             event_scheduler* parent);

  void bytesArrived(int vn, uint64_t unique_id, int bytes, message* parent);

};

DeclareFactory(packetizer, event_component*)



}
}

#endif // PACKETIZER_H
