#ifndef BUFFER_H
#define BUFFER_H

#include <sstmac/hardware/packet_flow/packet_flow_handler.h>
#include <sstmac/hardware/packet_flow/packet_flow_crossbar.h>
#include <sstmac/hardware/packet_flow/packet_flow_sender.h>

namespace sstmac {
namespace hw {


class packet_flow_buffer :
  public packet_flow_sender
{

 public:
  virtual ~packet_flow_buffer();

  virtual void
  set_output(sprockit::sim_parameters* params,
    int this_outport, int dst_inport,
    event_handler* output) override;

  virtual int
  queue_length() const {
    return 0;
  }

  event_loc_id
  output_location() const {
    return output_.handler->event_location();
  }

  event_loc_id
  input_location() const {
    return input_.handler->event_location();
  }

 protected:
  packet_flow_buffer(sprockit::sim_parameters* params, event_scheduler* parent);

  std::string
  buffer_string(const char* name) const;

 protected:
  packet_flow_input input_;
  packet_flow_output output_;
  long bytes_delayed_;

  static const int my_outport = 0;
  static const int my_inport = 0;
};

class packet_flow_finite_buffer :
  public packet_flow_buffer
{
 public:
  virtual ~packet_flow_finite_buffer(){}

  virtual void
  set_input(sprockit::sim_parameters* params,
            int this_inport, int src_outport,
            event_handler* input) override;

  packet_flow_finite_buffer(sprockit::sim_parameters* params,
                            event_scheduler* parent) :
    packet_flow_buffer(params, parent)
  {
  }

};

class packet_flow_infinite_buffer :
  public packet_flow_buffer
{
 protected:
  packet_flow_infinite_buffer(sprockit::sim_parameters* params,
                              event_scheduler* parent) :
   packet_flow_buffer(params, parent)
  {
  }

  virtual ~packet_flow_infinite_buffer(){}

  void //no-op, I don't need to send credits to an input, I'm infinite
  set_input(sprockit::sim_parameters* params, int my_inport, int dst_outport,
            event_handler *input) override {}

};

class packet_flow_network_buffer :
  public packet_flow_finite_buffer
{
 public:
  packet_flow_network_buffer(sprockit::sim_parameters* params,
                             event_scheduler* parent);

  virtual ~packet_flow_network_buffer();

  int
  queue_length() const override;

  void
  handle_credit(packet_flow_credit* msg) override;

  void
  do_handle_payload(packet_flow_payload* pkt) override;

  std::string
  packet_flow_name() const override {
    return "network buffer";
  }

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

 protected:
  int num_vc_;
  std::vector<payload_queue> queues_;
  std::vector<int> credits_;

 private:
  void build_blocked_messages();

 private:
  packet_flow_bandwidth_arbitrator* arb_;
  std::set<int> deadlocked_channels_;
  std::map<int, std::list<packet_flow_payload*> > blocked_messages_;
  bool queue_depth_reporting_;
  int queue_depth_delta_;
  int packet_size_;
};

class packet_flow_eject_buffer :
  public packet_flow_finite_buffer
{
 public:
  packet_flow_eject_buffer(sprockit::sim_parameters* params,
                           event_scheduler* parent) :
    packet_flow_finite_buffer(params, parent)
  {
  }

  void
  handle_credit(packet_flow_credit* msg) override;

  void
  return_credit(packet* msg);

  void
  do_handle_payload(packet_flow_payload* pkt) override;

  std::string
  packet_flow_name() const override {
    return "eject buffer";
  }

};

class packet_flow_injection_buffer :
  public packet_flow_infinite_buffer
{
 public:
  packet_flow_injection_buffer(sprockit::sim_parameters* params,
                               event_scheduler* parent);

  ~packet_flow_injection_buffer();

  int
  queue_length() const override;

  bool
  space_to_send(int bytes) const {
    return credits_ >= bytes;
  }

  void
  handle_credit(packet_flow_credit* msg) override;

  void
  do_handle_payload(packet_flow_payload* pkt) override;

  std::string
  packet_flow_name() const override {
    return "inject buffer";
  }

 protected:
  int packet_size_;
  packet_flow_bandwidth_arbitrator* arb_;
  long credits_;

};

}
}


#endif // BUFFER_H

