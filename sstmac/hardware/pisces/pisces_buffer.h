#ifndef pisces_BUFFER_H
#define pisces_BUFFER_H

#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_sender.h>

namespace sstmac {
namespace hw {


class pisces_buffer :
  public pisces_sender
{

 public:
  virtual ~pisces_buffer();

  virtual void
  set_output(sprockit::sim_parameters* params,
    int this_outport, int dst_inport,
    event_handler* output) override;

  virtual void
  set_input(
    sprockit::sim_parameters* params,
    int this_inport, int src_outport,
    event_handler* input) override;

  virtual int
  queue_length() const {
    return 0;
  }

  device_id
  output_location() const {
    return output_.handler->event_location();
  }

  device_id
  input_location() const {
    return input_.handler->event_location();
  }

 protected:
  pisces_buffer(sprockit::sim_parameters* params, event_scheduler* parent);

 protected:
  pisces_input input_;
  pisces_output output_;
  long bytes_delayed_;

  static const int my_outport = 0;
  static const int my_inport = 0;
};

class pisces_network_buffer :
  public pisces_buffer
{
 public:
  pisces_network_buffer(sprockit::sim_parameters* params,
                             event_scheduler* parent);

  virtual ~pisces_network_buffer();

  int
  queue_length() const override;

  void
  handle_credit(event* ev) override;

  void
  handle_payload(event* ev) override;

  std::string
  pisces_name() const override {
    return "network buffer";
  }

  event_handler*
  payload_handler();

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

 protected:
  int num_vc_;
  std::vector<payload_queue> queues_;
  std::vector<int> credits_;

 private:
  void build_blocked_messages();

 private:
  pisces_bandwidth_arbitrator* arb_;
  std::set<int> deadlocked_channels_;
  std::map<int, std::list<pisces_payload*> > blocked_messages_;
  int packet_size_;
  event_handler* payload_handler_;
};

class pisces_eject_buffer :
  public pisces_buffer
{
 public:
  pisces_eject_buffer(sprockit::sim_parameters* params,
                           event_scheduler* parent) :
    pisces_buffer(params, parent)
  {
  }

  void
  handle_credit(event* ev) override;

  void
  handle_payload(event* ev) override;

  void
  return_credit(packet* msg);

  std::string
  pisces_name() const override {
    return "eject buffer";
  }

};

class pisces_injection_buffer :
  public pisces_buffer
{
 public:
  pisces_injection_buffer(sprockit::sim_parameters* params,
                               event_scheduler* parent);

  ~pisces_injection_buffer();

  int
  queue_length() const override;

  bool
  space_to_send(int bytes) const {
    return credits_ >= bytes;
  }

  void
  handle_credit(event* ev) override;

  void
  handle_payload(event* ev) override;

  std::string
  pisces_name() const override {
    return "inject buffer";
  }

 protected:
  int packet_size_;
  pisces_bandwidth_arbitrator* arb_;
  long credits_;

};

}
}


#endif // BUFFER_H

