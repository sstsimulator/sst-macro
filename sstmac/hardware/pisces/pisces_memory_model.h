#ifndef pisces_MEMORY_MODEL_H
#define pisces_MEMORY_MODEL_H

#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/pisces/pisces_packetizer.h>

namespace sstmac {
namespace hw {



class memory_message : public message
{
  NotSerializable(memory_message)

 public:
  memory_message(long bytes, uint64_t id, double max_bw) :
    bytes_(bytes), id_(id), max_bw_(max_bw)
  {
  }

  long byte_length() const override {
    return bytes_;
  }

  uint64_t flow_id() const override {
    return id_;
  }

  std::string
  to_string() const override {
    return "memory message";
  }

  node_id
  toaddr() const override {
    return node_id();
  }

  node_id
  fromaddr() const override {
    return node_id();
  }

  double max_bw() const {
    return max_bw_;
  }

 private:
  uint64_t id_;
  long bytes_;
  double max_bw_;
};

class pisces_memory_packetizer : public packetizer
{
 public:
  pisces_memory_packetizer(sprockit::sim_parameters* params,
                          event_scheduler* parent);
  
  ~pisces_memory_packetizer();

  std::string
  to_string() const override {
    return "pisces memory packetizer";
  }

  link_handler* new_credit_handler() const override;
  link_handler* new_payload_handler() const override;

  void
  recv_credit(event* ev);

  void inject(int vn, long bytes, long byte_offset, message *payload) override;

  bool spaceToSend(int vn, int num_bits) override {
    return true;
  }

  double
  max_single_bw() const {
    return max_single_bw_;
  }

 private:
  void
  handle_payload(int vn, pisces_payload* pkt);

  void
  init_noise_model();

 private:
  double max_bw_;
  double max_single_bw_;
  timestamp latency_;
  pisces_bandwidth_arbitrator* arb_;
  noise_model* bw_noise_;
  noise_model* interval_noise_;
  int num_noisy_intervals_;
  packet_allocator* pkt_allocator_;
  event_handler* self_credit_handler_;

};


class pisces_memory_model :
  public memory_model,
  public packetizer_callback
{
 public:
  pisces_memory_model(sprockit::sim_parameters* params, node* nd);

  virtual ~pisces_memory_model();

  std::string
  to_string() const {
    return "packet flow memory model";
  }

  void
  schedule(timestamp t, event_handler *handler, message*msg){
    memory_model::schedule(t, handler, msg);
  }

  void notify(int vn, message* msg);

  virtual void
  access(long bytes, double max_bw, callback* cb);

  double
  max_single_bw() const {
    return mem_packetizer_->max_single_bw();
  }

 private:
  int allocate_channel();

 private:
  std::map<message*, callback*> pending_requests_;
  pisces_memory_packetizer* mem_packetizer_;
  std::list<int> channels_available_;
  int nchannels_;

};

}
} /* namespace sstmac */


#endif // pisces_MEMORY_MODEL_H
