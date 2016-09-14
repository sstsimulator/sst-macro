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

class packet_flow_bandwidth_arbitrator
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

  virtual int
  bytes_sending(timestamp now) const = 0;

 protected:
  packet_flow_bandwidth_arbitrator(sprockit::sim_parameters* params);

 protected:
  double out_bw_;
  double inv_out_bw_;

};

class packet_flow_null_arbitrator :
  public packet_flow_bandwidth_arbitrator
{
 public:
  packet_flow_null_arbitrator(sprockit::sim_parameters* params);

  virtual void
  arbitrate(packet_stats_st& st) override;

  std::string
  to_string() const override {
    return "packet_flow null arbitrator";
  }

  int
  bytes_sending(timestamp now) const override;

};


class packet_flow_simple_arbitrator :
  public packet_flow_bandwidth_arbitrator
{
 public:
  packet_flow_simple_arbitrator(sprockit::sim_parameters* params);

  virtual void
  arbitrate(packet_stats_st& st);

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
  typedef uint64_t ticks_t;
  typedef double bw_t;

 public:
  packet_flow_cut_through_arbitrator(sprockit::sim_parameters* params);

  ~packet_flow_cut_through_arbitrator();

  virtual void
  arbitrate(packet_stats_st& st);

  int
  bytes_sending(timestamp now) const;

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
  void clean_up(ticks_t now);

  void
  do_arbitrate(packet_stats_st& st);

  struct bandwidth_epoch {
    bw_t bw_available; //bandwidth is bytes per timestamp tick
    ticks_t start;
    ticks_t length;
    bandwidth_epoch* next;

    bandwidth_epoch() :
      next(nullptr) {
    }
    
    ~bandwidth_epoch(){
    }

    void truncate_after(ticks_t delta_t);

    void split(ticks_t delta_t);
  };

  bandwidth_epoch* head_;

  /** Convert from bytes/sec to bytes/tick */
  double bw_tick_to_sec_conversion_;
  double bw_sec_to_tick_conversion_;

};

DeclareFactory(packet_flow_bandwidth_arbitrator);

}
}

#endif // PACKETFLOW_ARBITRATOR_H

