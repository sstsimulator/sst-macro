#ifndef PACKET_FLOW_MEMORY_MODEL_H
#define PACKET_FLOW_MEMORY_MODEL_H

#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_sender.h>
#include <sstmac/hardware/packet_flow/packet_flow_packetizer.h>

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

  long byte_length() const {
    return bytes_;
  }

  uint64_t unique_id() const {
    return id_;
  }

  std::string
  to_string() const {
    return "memory message";
  }

  node_id
  toaddr() const {
    return node_id();
  }

  node_id
  fromaddr() const {
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

class packet_flow_memory_packetizer : public packet_flow_packetizer
{
 public:
  packet_flow_memory_packetizer();
  
  ~packet_flow_memory_packetizer();

  std::string
  packet_flow_name() const {
    return "packet flow memory model";
  }

  void
  recv_credit(packet_flow_credit* credit);

  void
  recv_packet(packet_flow_payload* pkt){}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  void finalize_init();

  void inject(int vn, long bytes, long byte_offset, message *payload);

  bool spaceToSend(int vn, int num_bits) const {
    return true;
  }

 private:
  void
  handle_payload(int vn, packet_flow_payload* pkt);

  void
  init_noise_model();

 private:
  double max_bw_;
  double max_single_bw_;
  timestamp latency_;
  packet_flow_bandwidth_arbitrator* arb_;
  noise_model* bw_noise_;
  noise_model* interval_noise_;
  int num_noisy_intervals_;
  packet_allocator* pkt_allocator_;

};

/**
  @class train_memorymodel
  A memory model compatible with the train message framework
*/
class packet_flow_memory_model :
  public memory_model,
  public packetizer_callback
{
 public:
  virtual ~packet_flow_memory_model();

  void
  set_event_parent(event_scheduler* m);

  virtual void
  finalize_init();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  void
  schedule(timestamp t, event_handler *handler, message*msg){
    memory_model::schedule(t, handler, msg);
  }

  void notify(int vn, message* msg);

  virtual void
  access(long bytes, double max_bw);

  double
  max_single_bw() const {
    return max_single_bw_;
  }

 private:
  int allocate_channel();

 private:
  //static int mtu_;
  double max_single_bw_;
  std::map<message*, sw::key*> pending_requests_;
  packet_flow_memory_packetizer* mem_packetizer_;
  std::list<int> channels_available_;
  int nchannels_;

};

}
} /* namespace sstmac */


#endif // PACKET_FLOW_MEMORY_MODEL_H
