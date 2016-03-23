#ifndef PACKETIZER_H
#define PACKETIZER_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/common/recv_cq.h>

namespace sstmac {
namespace hw {

class packetizer :
  public sprockit::factory_type,
  public event_subscheduler
{

 public:
  void start(int vn, message* payload);

  void packetArrived(packet* pkt);

  void sendWhatYouCan(int vn);

  virtual void setNotify(event_handler* handler){
    notifier_ = handler;
  }

  void notify(message* msg);

  int packetSize() const {
    return packet_size_;
  }

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

  event_handler* notifier_;

};

DeclareFactory(packetizer)

}
}

#endif // PACKETIZER_H
