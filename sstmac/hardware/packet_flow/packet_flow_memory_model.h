#ifndef PACKET_FLOW_MEMORY_MODEL_H
#define PACKET_FLOW_MEMORY_MODEL_H

#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/software/libraries/compute/compute_message_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_sender.h>
#include <sstmac/hardware/packet_flow/packet_flow_endpoint.h>

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

class packet_flow_memory_model;
class packet_flow_memory_system :
  public packet_flow_sender,
  public packet_flow_MTL
{
 public:
  packet_flow_memory_system(int mtu, packet_flow_memory_model* parent);
  
  ~packet_flow_memory_system();

  std::string
  packet_flow_name() const {
    return "packet flow memory model";
  }

  void
  do_handle_payload(packet_flow_payload* msg);

  void
  handle_credit(packet_flow_credit* msg);

  void
  init_credits(int port, int num_credits);

  int
  num_initial_credits() const;

  void
  set_input(int my_inport, int dst_outport, event_handler* input);

  void
  set_output(int my_outport, int dst_inport, event_handler* output);

  void start_message(message* msg){}

  virtual void
  init_params(sprockit::sim_parameters* params);

  void finalize_init();

  void init_noise_model();

  void set_event_parent(event_scheduler *m);

  void mtl_send(message* msg);

 private:
  void send_to_endpoint(timestamp t, packet_flow_payload* msg);

  int allocate_channel();

 private:
  double max_bw_;
  double max_single_bw_;
  timestamp latency_;
  packet_flow_bandwidth_arbitrator* arb_;
  packet_flow_endpoint* endpoint_;
  noise_model* bw_noise_;
  noise_model* interval_noise_;
  packet_flow_memory_model* parent_;
  int num_noisy_intervals_;

  struct pending_msg {
    long byte_offset;
    memory_message* msg;
  };
  std::vector<pending_msg> pending_;
  std::list<int> channels_available_;

};

/**
  @class train_memorymodel
  A memory model compatible with the train message framework
*/
class packet_flow_memory_model :
  public memory_model
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

  void
  handle(event *ev);

  virtual void
  access(long bytes, double max_bw);

  double
  max_single_bw() const {
    return max_single_bw_;
  }

 protected:
  void
  init_noise_model();

 private:
  //static int mtu_;
  double max_single_bw_;
  std::map<message*, sw::key*> pending_requests_;
  packet_flow_memory_system* mem_sys_;
};

}
} /* namespace sstmac */


#endif // PACKET_FLOW_MEMORY_MODEL_H
