#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>

#include <math.h>

ImplementFactory(sstmac::hw::packet_flow_bandwidth_arbitrator);

#define one_indent "  "
#define two_indent "    "

#define pflow_arb_debug_printf_l0(format_str, ...) \
  debug_printf(sprockit::dbg::packet_flow,  \
    " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_printf_l1(format_str, ...) \
  debug_printf(sprockit::dbg::packet_flow,  \
    one_indent " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_printf_l2(format_str, ...) \
  debug_printf(sprockit::dbg::packet_flow,  \
    two_indent " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_print_l2(format_str) \
  debug_printf(sprockit::dbg::packet_flow,  \
    two_indent " [arbitrator] " format_str "%s", "")

//#define pflow_arb_debug_printf_l0(format_str, ...) 
//#define pflow_arb_debug_printf_l1(format_str, ...) 
//#define pflow_arb_debug_printf_l2(format_str, ...) 
//#define pflow_arb_debug_print_l2(format_str) 

namespace sstmac {
namespace hw {

int packet_flow_cut_through_arbitrator::bandwidth_epoch::counter = 0;

SpktRegister("null", packet_flow_bandwidth_arbitrator,
            packet_flow_null_arbitrator,
            "Simple bandwidth arbitrator that models zero congestion on a link.");

SpktRegister("simple", packet_flow_bandwidth_arbitrator,
            packet_flow_simple_arbitrator,
            "Simple bandwidth arbitrator that only ever gives exclusive access to a link."
            "This corresponds to store-and-forward, which can be inaccurate for large packet sizes");

SpktRegister("cut_through", packet_flow_bandwidth_arbitrator,
            packet_flow_cut_through_arbitrator,
            "Bandwidth arbitrator that forwards packets as soon as they arrive and enough credits are received"
            "This is a much better approximation to wormhole or virtual cut_through routing");


static void
validate_bw(double test_bw)
{
  if (test_bw < 0 || (test_bw != test_bw)){ //i.e. NAN
    spkt_throw_printf(sprockit::value_error,
        "Payload has invalid bandwidth %12.8e",
        test_bw);
  }
}

packet_flow_bandwidth_arbitrator::packet_flow_bandwidth_arbitrator() :
  out_bw_(-1), inv_out_bw_(-1)
{
}

packet_flow_simple_arbitrator::packet_flow_simple_arbitrator() :
  next_free_(0)
{
}

void
packet_flow_bandwidth_arbitrator::partition(noise_model* noise, int num_intervals)
{
  spkt_throw_printf(sprockit::input_error,
    "%s is not compatible with partitioning",
    to_string().c_str());
}

void
packet_flow_bandwidth_arbitrator::init_noise_model(noise_model* noise)
{
  spkt_throw_printf(sprockit::input_error,
    "%s is not compatible with noise models",
    to_string().c_str());
}

void
packet_flow_simple_arbitrator::arbitrate(packet_stats_st &st)
{
  timestamp start_send = next_free_ < st.now ? st.now : next_free_;
  timestamp ser_delay(st.pkt->num_bytes() * inv_out_bw_);
  next_free_ = start_send + ser_delay;
  st.pkt->set_bw(out_bw_);
  //store and forward
  //head/tail are linked and go "at same time"
  st.head_leaves = st.tail_leaves = next_free_;
  //we can send the credit a bit ahead of time
  st.credit_leaves = st.head_leaves
    + credit_delay(st.pkt->max_incoming_bw(), out_bw_, st.pkt->num_bytes());
  st.pkt->set_max_incoming_bw(out_bw_);
}

int
packet_flow_simple_arbitrator::bytes_sending(timestamp now) const
{
  double send_delay = next_free_ > now ? (next_free_ - now).sec() : 0;
  int bytes_sending = send_delay * out_bw_;
  return bytes_sending;
}

packet_flow_null_arbitrator::packet_flow_null_arbitrator()
{
}

void
packet_flow_null_arbitrator::arbitrate(packet_stats_st &st)
{
  st.pkt->set_max_bw(out_bw_);
  timestamp ser_delay(st.pkt->num_bytes() / st.pkt->bw());
  st.head_leaves = st.now;
  st.tail_leaves = st.now + ser_delay;
  //we can send the credit a bit ahead of the tail
  st.credit_leaves = st.head_leaves
    + credit_delay(st.pkt->max_incoming_bw(), out_bw_, st.pkt->num_bytes());
  st.pkt->set_max_incoming_bw(out_bw_);
}

int
packet_flow_null_arbitrator::bytes_sending(timestamp now) const
{
  return 0;
}

packet_flow_cut_through_arbitrator::packet_flow_cut_through_arbitrator()
  : head_(0)
{
}


void
packet_flow_cut_through_arbitrator::set_outgoing_bw(double out_bw)
{
  pflow_arb_debug_printf_l0("initializing cut through arbitrator with bw=%8.4e", out_bw);
  packet_flow_bandwidth_arbitrator::set_outgoing_bw(out_bw);
  head_ = new bandwidth_epoch;
  head_->bw_available = out_bw;
  head_->start = 0;
  //just set to super long
  head_->length = 1e30;

}

packet_flow_cut_through_arbitrator::~packet_flow_cut_through_arbitrator()
{
  bandwidth_epoch* next = head_;
  while (next){
    bandwidth_epoch* e = next;
    next = next->next;
    delete e;
  }
}

int
packet_flow_cut_through_arbitrator::bytes_sending(timestamp now) const
{
  double next_free =
    head_->start; //just assume that at head_->start link is fully available
  double now_ = now.sec();
  double send_delay = next_free > now_ ? (next_free - now_) : 0;
  int bytes_sending = send_delay * out_bw_;
  return bytes_sending;
}

void
packet_flow_cut_through_arbitrator::partition(noise_model* noise, int num_intervals)
{
  //find the tail
  bandwidth_epoch* tail = head_;
  while (tail->next) //while true, not the tail
    tail = tail->next;

  for (int i=0; i < num_intervals; ++i){
    tail->split(noise->value());
    tail = tail->next;
  }

}

void
packet_flow_cut_through_arbitrator::init_noise_model(noise_model* noise)
{
  bandwidth_epoch* next = head_;
  while (next){
    next->bw_available = noise->value();
    next = next->next;
  }
}

void
packet_flow_cut_through_arbitrator::bandwidth_epoch::split(double delta_t)
{
  bandwidth_epoch* new_epoch = new bandwidth_epoch;
  new_epoch->bw_available = this->bw_available;
  new_epoch->start = this->start + delta_t;
  new_epoch->length = this->length - delta_t;
  new_epoch->next = this->next;
  this->next = new_epoch;
  this->length = delta_t;
}

void
packet_flow_cut_through_arbitrator::bandwidth_epoch::truncate_after(
  double delta_t)
{
  start += delta_t;
  length -= delta_t;
}

void
packet_flow_cut_through_arbitrator::clean_up(double now)
{
  bandwidth_epoch* epoch = head_;
  while (1) {

    if (epoch->start >= now) {
      return; // we are done
    }

    double delta_t = now - epoch->start;
    if (delta_t >= epoch->length) { //this epoch has expired
      //delete and move on
      head_ = epoch->next;
      delete epoch;
      epoch = head_;
    }
    else { //we are in the middle of this epoch
      epoch->truncate_after(delta_t);
      return; //we are done
    }

    if (!head_) {
      spkt_throw_printf(sprockit::illformed_error, "head is null");
    }
  }
}

void
packet_flow_cut_through_arbitrator::arbitrate(packet_stats_st &st)
{
  do_arbitrate(st);
  st.head_leaves.correct_round_off(st.now);
  //we can send the credit a bit ahead of the tail
  st.credit_leaves = st.head_leaves
    + credit_delay(st.pkt->max_incoming_bw(), out_bw_, st.pkt->num_bytes());
  st.pkt->set_max_incoming_bw(out_bw_);
}

void
packet_flow_cut_through_arbitrator::do_arbitrate(packet_stats_st &st)
{
  packet_flow_payload* payload = st.pkt;

  payload->init_bw(out_bw_);
#if SSTMAC_SANITY_CHECK
  validate_bw(payload->bw());
#endif

  //first things first - clean out any old epochs
  double now = st.now.sec();
  clean_up(now);

  double send_start = head_->start;

  long bytes_queued = payload->bw() * (send_start - payload->arrival());
#if SSTMAC_SANITY_CHECK
  if (bytes_queued < 0) {
    spkt_throw_printf(sprockit::value_error,
                     "Payload has negative number of bytes queued: bw=%12.8e send_start=%20.16e arrival=%20.16e",
                     payload->bw(), send_start, payload->arrival());
  }
#endif
  //zero byte packets break the math below - if tiny, just push it up to 8
  int bytes_to_send = std::max(payload->num_bytes(), 8);

  pflow_arb_debug_printf_l0("cut_through arbitrator handling %s at time %10.5e that started arriving at %10.5e",
        payload->to_string().c_str(), st.now, payload->arrival());

  bandwidth_epoch* epoch = head_;
  //bandwidth sanity check - available bw should only ever go up in future epochs
  double last_epoch_bw = 0;
  while (1) {
    pflow_arb_debug_printf_l1("epoch BW=%9.5e start=%9.5e length=%9.5e: bytes_to_send=%d bytes_queued=%d",
                           epoch->bw_available,
                           epoch->start,
                           epoch->length,
                           bytes_to_send,
                           bytes_queued);

    double delta_bw = payload->bw() - epoch->bw_available;

    /**
        This is basically assuming payload->bw >= bw_available
        We use the -1 to avoid huge numbers in the else block
        We are maximally utilizing all bw available
    */
    if (delta_bw > -1.) {
      //see if we can send all the bytes in this epoch
      double time_to_send = bytes_to_send / epoch->bw_available;
      pflow_arb_debug_printf_l2("delta=%8.4e, send_time=%8.4e using all available bandwidth in epoch",
                             delta_bw, time_to_send);
      if (time_to_send < epoch->length) {
        double payload_stop = epoch->start + time_to_send;
        double total_send_time = payload_stop - send_start;
        double new_bw = payload->num_bytes() / total_send_time;
#if SSTMAC_SANITY_CHECK
        validate_bw(new_bw);
#endif
        payload->set_bw(new_bw);
        epoch->truncate_after(time_to_send);
        pflow_arb_debug_printf_l1("truncate epoch: start=%12.8e stop=%12.8e send_time=%12.8e new_bw=%12.8e",
                                send_start, payload_stop, total_send_time, new_bw);
        st.head_leaves = timestamp(send_start);
        st.tail_leaves = timestamp(payload_stop);
        return;
      }
      else if (time_to_send == epoch->length) {
        double payload_stop = epoch->start + time_to_send;
        double total_send_time = payload_stop - send_start;
        double new_bw = payload->num_bytes() / total_send_time;
        payload->set_bw(new_bw);
#if SSTMAC_SANITY_CHECK
        validate_bw(new_bw);
#endif
        head_ = epoch->next;
        delete epoch;
        pflow_arb_debug_printf_l2("exact fit: start=%12.8e stop=%12.8e send_time=%12.8e new_bw=%12.8e\n",
                                   send_start, payload_stop, total_send_time, new_bw);
        st.head_leaves = timestamp(send_start);
        st.tail_leaves = timestamp(payload_stop);
        return;
      }
      else {
        //this epoch is exhausted
        bytes_to_send -= epoch->bw_available * epoch->length;
        head_ = epoch->next;
        delete epoch;
        epoch = head_;
        pflow_arb_debug_print_l2("epoch used up");
      }
    }

    /**
        The payload is sending slower than the max available bw
        We are not complicated by any bytes being arrived in the queue
    */
    else if (bytes_queued == 0) {
      //we are under-utilizing the bandwidth
      double time_to_send = bytes_to_send / payload->bw();
      pflow_arb_debug_printf_l2("underutilized, No Queue: time_to_send=%12.8e",
                                time_to_send);

      if (time_to_send <= epoch->length) {
        epoch->split(time_to_send);
        epoch->bw_available -= payload->bw();

        //configure bandwidth
        double send_done = epoch->start + time_to_send;
        double new_bw = payload->num_bytes() / (send_done - send_start);
#if SSTMAC_SANITY_CHECK
        validate_bw(new_bw);
#endif
        payload->set_bw(new_bw);
        pflow_arb_debug_printf_l2("send finishes: start=%12.8e stop=%12.8e new_bw=%12.8e",
                               send_start, send_done, new_bw);
        st.head_leaves = timestamp(send_start);
        st.tail_leaves = timestamp(send_done);
        return;
      }
      else {
#if SSTMAC_SANITY_CHECK
        if (epoch->next ==
            0) { //we should never be subtracting from the big long epoch at the end
          spkt_throw_printf(sprockit::illformed_error,
                           "Subtracting bandwidth from the final epoch:\n"
                           "bytes_to_send=%d\n"
                           "payload_bw=%20.16e\n"
                           "time_to_send=%20.16e\n"
                           "epoch_length=%20.16e\n",
                           bytes_to_send,
                           payload->bw(),
                           time_to_send,
                           epoch->length);
        }
#endif
        epoch->bw_available -= payload->bw();
        bytes_to_send -= payload->bw() * epoch->length;
        epoch = epoch->next;
        pflow_arb_debug_print_l2("send not done yet");
      }
    }


    /**
        The payload is sending slower than the max available bandwidth
        However, we have a certain number of bytes that are instantly ready to go in the queue
    */
    else {
      //the number of bytes available to send is the line
      // BA = INP * t + QUE
      //the number bytes that could have been sent is
      // BS = OUT * t
      //we want to know where lines intersect
      //we need to solve BS = BA => OUT * t = INP * t + QUE
      //subject to t >= 0

      //the intersection might come after whole message is sent
      double send_all_time = bytes_to_send / epoch->bw_available;
      double time_to_intersect = bytes_queued / (-delta_bw);
      double time_to_send = std::min(epoch->length, std::min(send_all_time,
                                     time_to_intersect));

      pflow_arb_debug_printf_l2("underutilized, but %d bytes queued: delta=%12.8e "
                             "send_all_time=%12.8e time_to_intersect=%12.8e time_to_send=%12.8e",
                             bytes_queued, delta_bw, send_all_time, time_to_intersect, time_to_send);

      if (time_to_send == send_all_time) {
        //and the message completely finishes
        double send_done = epoch->start + time_to_send;
        double new_bw = payload->num_bytes() / (send_done - send_start);
        payload->set_bw(new_bw);
#if SSTMAC_SANITY_CHECK
        validate_bw(new_bw);
#endif
        pflow_arb_debug_printf_l2("send finishes: start=%12.8e stop=%12.8e new_bw=%12.8e",
                               send_start, send_done, new_bw);
        epoch->truncate_after(time_to_send);
        st.head_leaves = timestamp(send_start);
        st.tail_leaves = timestamp(send_done);
        return;
      }
      else if (time_to_send == epoch->length) {
#if SSTMAC_SANITY_CHECK
        if (epoch->next == 0) {
          //something freaked out numerically
          //time_to_send should never equal the length of the big long, last epoch
          spkt_throw_printf(sprockit::illformed_error,
                           "Time to send packet_flow is way too long:\n"
                           "send_all_time=%20.16e\n"
                           "time_to_intersect=%20.16e\n"
                           "epoch_length=%20.16e\n"
                           "time_to_send=%20.16e\n"
                           "bytes_to_send=%d\n"
                           "bytes_queued=%d\n"
                           "bw_available=%20.16e\n",
                           "payload_bw=%20.16e\n",
                           "delta_bw=%20.16e\n",
                           send_all_time,
                           time_to_intersect,
                           epoch->length,
                           time_to_send,
                           bytes_to_send,
                           bytes_queued,
                           epoch->bw_available,
                           payload->bw(),
                           delta_bw);
        }
#endif
        //add in the contributions
        bytes_to_send -= ceil(epoch->bw_available * time_to_send);
        //we are also draining the queue
        //delta < 0 so this is really a subtraction
        long save_bytes_queued = bytes_queued;
        bytes_queued += ceil(delta_bw * epoch->length);
#if SSTMAC_SANITY_CHECK
        if (bytes_queued < 0) {
          spkt_throw_printf(sprockit::value_error,
                           "Payload now has negative number of bytes queued:\n"
                           "payload_bw=%20.16e\n"
                           "epoch_bw=%20.16e\n"
                           "delta_bw=%20.16e\n"
                           "epoch_length=%20.16e\n"
                           "bytes_queued=%d\n",
                           payload->bw(),
                           epoch->bw_available,
                           delta_bw,
                           epoch->length,
                           save_bytes_queued);
        }
#endif
        //this epoch is exhausted
        head_ = epoch->next;
        delete epoch;
        epoch = head_;
        pflow_arb_debug_print_l2("send not done yet");
      }

      else { //time_to_send = time_to_intersect
        //the queue is completely drained during the epoch
        epoch->truncate_after(time_to_send);
        //but we are not done yet - add the contributions
        bytes_to_send -= ceil(epoch->bw_available * time_to_send);
        bytes_queued = 0;
        pflow_arb_debug_print_l2("queue emptied");
      }

    }

    if (!head_) {
      spkt_throw_printf(sprockit::illformed_error, "head is null");
    }

  }
}

}
}

