#ifndef PACKETFLOW_ARBITRATOR_H
#define PACKETFLOW_ARBITRATOR_H

#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/factories/factory.h>
#include <sstmac/hardware/noise/noise.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats.h>

namespace sstmac {
namespace hw {

class packet_flow_bandwidth_arbitrator :
  public sprockit::factory_type
{

 public:
  /**
      Assign bandwidth to payload.
      @return The time at which the packet can be forwarded to the next switch/node/etc.
  */
  virtual void
  arbitrate(packet_stats_st& st) = 0;

  virtual std::string
  to_string() const = 0;

  virtual ~packet_flow_bandwidth_arbitrator(){}

  virtual void
  set_outgoing_bw(double out_bw) {
    out_bw_ = out_bw;
    inv_out_bw_ = 1.0 / out_bw;
  }

  double outgoing_bw() const {
    return out_bw_;
  }

  static inline timestamp
  credit_delay(double max_in_bw, double out_bw, long bytes){
    double credit_delta = 1.0/out_bw - 1.0/max_in_bw;
    credit_delta = std::max(0., credit_delta);
    return timestamp(bytes * credit_delta);
  }

  virtual void
  init_noise_model(noise_model* noise);

  /**
   * @brief partition Partition the arbitrator time windows into a series of randomly sized chunks
   * @param noise  The noise model that randomly selects time values
   * @param num_intervals
   */
  virtual void
  partition(noise_model* noise,
    int num_intervals);

  virtual packet_flow_bandwidth_arbitrator*
  clone(double bw) const = 0;

  virtual int
  bytes_sending(timestamp now) const = 0;

 protected:
  packet_flow_bandwidth_arbitrator();

 protected:
  double out_bw_;
  double inv_out_bw_;

};

class packet_flow_null_arbitrator :
  public packet_flow_bandwidth_arbitrator
{
 public:
  packet_flow_null_arbitrator();

  virtual void
  arbitrate(packet_stats_st& st);

  virtual packet_flow_bandwidth_arbitrator*
  clone(double bw) const {
    packet_flow_null_arbitrator* arb = new packet_flow_null_arbitrator;
    arb->set_outgoing_bw(bw);
    return arb;
  }

  std::string
  to_string() const {
    return "packet_flow null arbitrator";
  }

  int
  bytes_sending(timestamp now) const;

};


class packet_flow_simple_arbitrator :
  public packet_flow_bandwidth_arbitrator
{
 public:
  packet_flow_simple_arbitrator();

  virtual void
  arbitrate(packet_stats_st& st);

  virtual packet_flow_bandwidth_arbitrator*
  clone(double bw) const {
    packet_flow_simple_arbitrator* arb = new packet_flow_simple_arbitrator;
    arb->set_outgoing_bw(bw);
    return arb;
  }

  std::string
  to_string() const {
    return "packet_flow simple arbitrator";
  }

  int
  bytes_sending(timestamp now) const;

 protected:
  timestamp next_free_;

};

class packet_flow_cut_through_arbitrator :
  public packet_flow_bandwidth_arbitrator
{

 public:
  packet_flow_cut_through_arbitrator();

  ~packet_flow_cut_through_arbitrator();

  virtual void
  arbitrate(packet_stats_st& st);

  virtual void
  set_outgoing_bw(double bw);

  int
  bytes_sending(timestamp now) const;

  packet_flow_bandwidth_arbitrator*
  clone(double bw) const {
    packet_flow_bandwidth_arbitrator* new_arb =
      new packet_flow_cut_through_arbitrator;
    new_arb->set_outgoing_bw(bw);
    return new_arb;
  }

  std::string
  to_string() const {
    return "cut through arbitrator";
  }

  void
  partition(noise_model* model,
    int num_intervals);

  void
  init_noise_model(noise_model* noise);

 private:
  void clean_up(double now);

  void
  do_arbitrate(packet_stats_st& st);

  struct bandwidth_epoch {
    double bw_available;
    double start;
    double length;
    bandwidth_epoch* next;

    bandwidth_epoch() :
      next(0) {
      counter_ = counter++;
      //printf("allocating epoch %d\n", counter_);
    }
    
    ~bandwidth_epoch(){
      //printf("deleting epoch %d\n", counter_);
    }

    static int counter;

    int counter_;

    void truncate_after(double delta_t);

    void split(double delta_t);
  };

  bandwidth_epoch* head_;


};

DeclareFactory(packet_flow_bandwidth_arbitrator);

}
}

#endif // PACKETFLOW_ARBITRATOR_H

