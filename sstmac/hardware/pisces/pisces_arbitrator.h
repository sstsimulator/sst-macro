#ifndef PACKETFLOW_ARBITRATOR_H
#define PACKETFLOW_ARBITRATOR_H

#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/factories/factory.h>
#include <sstmac/hardware/noise/noise.h>
#include <sstmac/hardware/pisces/pisces_stats.h>

namespace sstmac {
namespace hw {

/**
 * @brief The pisces_bandwidth_arbitrator class  This arbitrates the available bandwidth
 *        amongst incoming packets. This can either occur on discrete packets or it can
 *        attempt to simulate flits.
 */
class pisces_bandwidth_arbitrator
{
 public:
  /**
      Assign bandwidth to payload.
      @return The time at which the packet can be forwarded to the next switch/node/etc.
  */
  virtual void
  arbitrate(pkt_arbitration_t& st) = 0;

  virtual timestamp
  head_tail_delay(pisces_payload* pkt) = 0;

  virtual std::string
  to_string() const = 0;

  virtual ~pisces_bandwidth_arbitrator(){}

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
  pisces_bandwidth_arbitrator(sprockit::sim_parameters* params);

 protected:
  double out_bw_;
  double inv_out_bw_;

};

/**
 * @brief The pisces_null_arbitrator class  The performs no congestion modeling.
 *        This assumes packets can always receive full bandwidth regardless of traffic.
 */
class pisces_null_arbitrator :
  public pisces_bandwidth_arbitrator
{
 public:
  pisces_null_arbitrator(sprockit::sim_parameters* params);

  virtual void
  arbitrate(pkt_arbitration_t& st) override;

  std::string
  to_string() const override {
    return "pisces null arbitrator";
  }

  timestamp
  head_tail_delay(pisces_payload *pkt) override;

  int
  bytes_sending(timestamp now) const override;

};

/**
 * @brief The pisces_simple_arbitrator class Implements a store-and-forward arbitration scheme.
 * The entire packet has to arrive before it can be arbitrated.  Links/crossbars never interleave
 * flits from different packets in this scheme.  For larger packet sizes, this can lead to
 * unrealistic delays since packets cannot pipeline across network stages.
 */
class pisces_simple_arbitrator :
  public pisces_bandwidth_arbitrator
{
 public:
  pisces_simple_arbitrator(sprockit::sim_parameters* params);

  virtual void
  arbitrate(pkt_arbitration_t& st) override;

  std::string
  to_string() const override {
    return "pisces simple arbitrator";
  }

  timestamp
  head_tail_delay(pisces_payload *pkt) override {
    //no delay
    return timestamp(0,timestamp::exact);
  }

  int
  bytes_sending(timestamp now) const override;

 protected:
  timestamp next_free_;

};

/**
 * @brief The pisces_simple_arbitrator class Implements a store-and-forward arbitration scheme.
 * The entire packet has to arrive before it can be arbitrated.  Links/crossbars never interleave
 * flits from different packets in this scheme.  For larger packet sizes, this can lead to
 * unrealistic delays since packets cannot pipeline across network stages.
 */
class pisces_cut_through_arbitrator :
  public pisces_bandwidth_arbitrator
{
  typedef uint64_t ticks_t;
  typedef double bw_t;

 public:
  pisces_cut_through_arbitrator(sprockit::sim_parameters* params);

  ~pisces_cut_through_arbitrator();

  virtual void
  arbitrate(pkt_arbitration_t& st) override;

  int
  bytes_sending(timestamp now) const override;

  std::string
  to_string() const override {
    return "cut through arbitrator";
  }

  void
  partition(noise_model* model,
    int num_intervals) override;

  void
  init_noise_model(noise_model* noise) override;

  timestamp
  head_tail_delay(pisces_payload *pkt) override;

 private:
  void clean_up(ticks_t now);

  void
  do_arbitrate(pkt_arbitration_t& st);

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

DeclareFactory(pisces_bandwidth_arbitrator);

}
}

#endif // PACKETFLOW_ARBITRATOR_H

