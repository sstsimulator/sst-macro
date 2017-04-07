#ifndef PACKETFLOW_CROSSBAR_H
#define PACKETFLOW_CROSSBAR_H

#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/keyword_registration.h>

#include <memory>

#define identity_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::identity_mapper())
#define constant_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::constant_mapper(n))
#define offset_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::offset_mapper(n))
#define divide_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::div_mapper(n))
#define mod_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::mod_mapper(n))

namespace sstmac {
namespace hw {

class pisces_NtoM_queue :
  public pisces_sender
{
 public:
  virtual ~pisces_NtoM_queue();

  pisces_NtoM_queue(sprockit::sim_parameters* params,
                         event_scheduler* parent);

  int
  thread_id() const {
    return event_subcomponent::thread_id();
  }

  void
  handle_payload(event* ev) override;

  void
  handle_credit(event* ev) override;

  event_handler*
  credit_handler();

  event_handler*
  payload_handler();

  void
  set_input(sprockit::sim_parameters* params,
            int my_inport, int src_outport, event_handler* input) override;

  void
  set_output(sprockit::sim_parameters* params,
             int my_outport, int dst_inport, event_handler* output) override;

  virtual void
  start_message(message* msg);

  inline int
  slot(int port, int vc) const {
    return port * num_vc_ + vc;
  }

  void
  set_tile_id(std::string id) {
    tile_id_ = id;
  }

  std::string tile_id() const {
    return tile_id_;
  }

  void
  deadlock_check() override;

  void
  deadlock_check(event* ev) override;

  class port_mapper
  {
  public:
    port_mapper() {}
    virtual ~port_mapper() {}
    virtual int local_port(const int) const = 0;
  };

  class identity_mapper : public port_mapper
  {
  private:
  public:
    identity_mapper() {}
    ~identity_mapper() {}
    virtual int local_port(const int port) const override {
      return port;
    }
  };

  class constant_mapper : public port_mapper
  {
  private:
    int constant_;
  public:
    constant_mapper(int constant) : constant_(constant) {}
    ~constant_mapper() {}
    virtual int local_port(const int port) const override {
      return constant_;
    }
  };

  class offset_mapper : public port_mapper
  {
  private:
    int offset_;
  public:
    offset_mapper(int offset) : offset_(offset)  {}
    ~offset_mapper() {}
    virtual int local_port(const int port) const override {
      return port - offset_;
    }
  };

  class div_mapper : public port_mapper
  {
  private:
    int div_;
  public:
    div_mapper(int div) : div_(div) {
    }
    ~div_mapper() {}
    virtual int local_port(const int port) const override {
      return port / div_;
    }
  };

  class mod_mapper : public port_mapper
  {
  private:
    int mod_;
  public:
    mod_mapper(int mod) : mod_(mod) {}
    ~mod_mapper() {}
    virtual int local_port(const int port) const override {
      return port % mod_;
    }
  };

  void
  configure_outports(int num_ports,
                     std::unique_ptr<port_mapper> mapper
                     = std::unique_ptr<port_mapper>(new identity_mapper()),
                     std::unique_ptr<port_mapper> credit_mapper
                     = std::unique_ptr<port_mapper>(new identity_mapper()) ) {
    resize(num_ports);
    outport_mapper_ = std::move(mapper);
    credit_mapper_ = std::move(credit_mapper);
  }

  int
  local_outport(int port) {
    return outport_mapper_->local_port(port);
  }

  int
  local_outport_credit(int port) {
    return credit_mapper_->local_port(port);
  }

 protected:
  typedef spkt_unordered_map<int, pisces_input> input_map;

  typedef std::vector<pisces_output> output_map;
  typedef std::vector<int> credit_map;
  typedef std::vector<payload_queue> queue_map;

  pisces_bandwidth_arbitrator* arb_;

  input_map inputs_;
  output_map outputs_;
  //indexed by slot number = (port,vc)
  credit_map credits_;
  //indexed by slot number = (port,vc)
  queue_map queues_;

  int num_vc_;
  int port_offset_;
  int port_div_;
  int port_mod_;

  event_handler* credit_handler_;
  event_handler* payload_handler_;

  std::map<int, std::set<int> > deadlocked_channels_;

  std::map<int, std::map<int, std::list<pisces_payload*> > > blocked_messages_;

  std::string tile_id_;

 protected:
  void
  send_payload(pisces_payload* pkt);

  void
  build_blocked_messages();

 private:
  std::unique_ptr<port_mapper> outport_mapper_;
  std::unique_ptr<port_mapper> credit_mapper_;

  inline int& credit(int port, int vc){
    return credits_[slot(port, vc)];
  }

  void resize(int num_ports);

  inline payload_queue& queue(int port, int vc){
    return queues_[slot(port, vc)];
  }

  std::string
  input_name(pisces_payload* pkt);

  std::string
  output_name(pisces_payload* pkt);

  event_handler*
  output_handler(pisces_payload* pkt);

};

class pisces_demuxer :
  public pisces_NtoM_queue
{
 public:
  pisces_demuxer(sprockit::sim_parameters* params,
                      event_scheduler* parent);
  std::string
  pisces_name() const override {
    return "demuxer" + tile_id();
  }
};


class pisces_muxer :
  public pisces_NtoM_queue
{
 public:
  pisces_muxer(sprockit::sim_parameters* params,
                    event_scheduler* parent);
  std::string
  pisces_name() const override {
    return "muxer" + tile_id();
  }
};

class pisces_crossbar :
  public pisces_NtoM_queue
{
 public:
  pisces_crossbar(sprockit::sim_parameters* params,
                       event_scheduler* parent);
  std::string
  pisces_name() const override {
    return "crossbar" + tile_id();
  }
};


}
}

#endif // PACKETFLOW_CROSSBAR_H

