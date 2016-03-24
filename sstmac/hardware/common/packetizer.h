#ifndef PACKETIZER_H
#define PACKETIZER_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/common/recv_cq.h>

namespace sstmac {
namespace hw {

class packetizer_callback
{
 public:
  virtual void notify(int vn, message* msg) = 0;
};

class packetizer :
  public sprockit::factory_type,
  public event_subscheduler
{

 public:
  void start(int vn, message* payload);

  void packetArrived(int vn, packet* pkt);

  void sendWhatYouCan(int vn);

  void setNotify(packetizer_callback* handler){
    notifier_ = handler;
  }

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

  packetizer_callback* notifier_;

};

DeclareFactory(packetizer)

}
}

#endif // PACKETIZER_H
