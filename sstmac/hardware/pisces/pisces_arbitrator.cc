/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/hardware/pisces/pisces_arbitrator.h>

#include <math.h>

#define one_indent "  "
#define two_indent "    "

#define pflow_arb_debug_printf_l0(format_str, ...) \
  debug_printf(sprockit::dbg::pisces,  \
    " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_printf_l1(format_str, ...) \
  debug_printf(sprockit::dbg::pisces,  \
    one_indent " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_printf_l2(format_str, ...) \
  debug_printf(sprockit::dbg::pisces,  \
    two_indent " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_print_l2(format_str) \
  debug_printf(sprockit::dbg::pisces,  \
    two_indent " [arbitrator] " format_str "%s", "")

//#define pflow_arb_debug_printf_l0(format_str, ...) 
//#define pflow_arb_debug_printf_l1(format_str, ...) 
//#define pflow_arb_debug_printf_l2(format_str, ...) 
//#define pflow_arb_debug_print_l2(format_str) 

namespace sstmac {
namespace hw {

static void
validate_bw(double test_bw)
{
  if (test_bw < 0 || (test_bw != test_bw)){ //i.e. NAN
    spkt_throw_printf(sprockit::value_error,
        "Payload has invalid bandwidth %12.8e",
        test_bw);
  }
}

pisces_bandwidth_arbitrator::
pisces_bandwidth_arbitrator(sprockit::sim_parameters* params)
{
  out_bw_ = params->get_bandwidth_param("bandwidth");
  if (out_bw_ < 1){
    spkt_abort_printf("Got erroneously low bandwidth %f in arbitrator from param %s", 
      out_bw_, params->get_param("bandwidth").c_str());
  }
  inv_out_bw_ = 1.0 / out_bw_;
}

pisces_simple_arbitrator::pisces_simple_arbitrator(sprockit::sim_parameters* params) :
  next_free_(0),
  pisces_bandwidth_arbitrator(params)
{
}

void
pisces_bandwidth_arbitrator::partition(noise_model* noise, int num_intervals)
{
  spkt_throw_printf(sprockit::input_error,
    "%s is not compatible with partitioning",
    to_string().c_str());
}

void
pisces_bandwidth_arbitrator::init_noise_model(noise_model* noise)
{
  spkt_throw_printf(sprockit::input_error,
    "%s is not compatible with noise models",
    to_string().c_str());
}

void
pisces_simple_arbitrator::arbitrate(pkt_arbitration_t &st)
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
pisces_simple_arbitrator::bytes_sending(timestamp now) const
{
  double send_delay = next_free_ > now ? (next_free_ - now).sec() : 0;
  int bytes_sending = send_delay * out_bw_;
  return bytes_sending;
}

pisces_null_arbitrator::pisces_null_arbitrator(sprockit::sim_parameters* params) :
  pisces_bandwidth_arbitrator(params)
{
}

timestamp
pisces_null_arbitrator::head_tail_delay(pisces_payload *pkt)
{
  timestamp ser_delay(pkt->num_bytes() / pkt->bw());
  return ser_delay;
}

void
pisces_null_arbitrator::arbitrate(pkt_arbitration_t &st)
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
pisces_null_arbitrator::bytes_sending(timestamp now) const
{
  return 0;
}

pisces_cut_through_arbitrator::
pisces_cut_through_arbitrator(sprockit::sim_parameters* params)
  : head_(nullptr),
    pisces_bandwidth_arbitrator(params)
{
  timestamp sec(1.0);
  timestamp tick(1, timestamp::exact);
  bw_sec_to_tick_conversion_ = tick.sec();
  bw_tick_to_sec_conversion_ = sec.ticks_int64();

  head_ = new bandwidth_epoch;
  head_->bw_available = out_bw_ * bw_sec_to_tick_conversion_;
  head_->start = 0;
  //just set to super long
  head_->length = std::numeric_limits<uint64_t>::max();
}


timestamp
pisces_cut_through_arbitrator::head_tail_delay(pisces_payload *pkt)
{
  timestamp ser_delay(pkt->num_bytes() / pkt->bw());
  return ser_delay;
}

pisces_cut_through_arbitrator::~pisces_cut_through_arbitrator()
{
  bandwidth_epoch* next = head_;
  while (next){
    bandwidth_epoch* e = next;
    next = next->next;
    delete e;
  }
}

int
pisces_cut_through_arbitrator::bytes_sending(timestamp now) const
{
  double next_free =
    head_->start; //just assume that at head_->start link is fully available
  double now_ = now.sec();
  double send_delay = next_free > now_ ? (next_free - now_) : 0;
  int bytes_sending = send_delay * out_bw_;
  return bytes_sending;
}

void
pisces_cut_through_arbitrator::partition(noise_model* noise, int num_intervals)
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
pisces_cut_through_arbitrator::init_noise_model(noise_model* noise)
{
  bandwidth_epoch* next = head_;
  while (next){
    next->bw_available = noise->value();
    next = next->next;
  }
}

void
pisces_cut_through_arbitrator::bandwidth_epoch::split(ticks_t delta_t)
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
pisces_cut_through_arbitrator::bandwidth_epoch::truncate_after(ticks_t delta_t)
{
  ticks_t finish = start + length;
  start += delta_t;

  length -= delta_t;
}

void
pisces_cut_through_arbitrator::clean_up(ticks_t now)
{
  bandwidth_epoch* epoch = head_;
  while (1) {

    if (epoch->start >= now) {
      return; // we are done
    }

    ticks_t delta_t = now - epoch->start;
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
pisces_cut_through_arbitrator::arbitrate(pkt_arbitration_t &st)
{
  do_arbitrate(st);
  st.head_leaves.correct_round_off(st.now);
  //we can send the credit a bit ahead of the tail
  st.credit_leaves = st.head_leaves
    + credit_delay(st.pkt->max_incoming_bw(), out_bw_, st.pkt->num_bytes());
  st.pkt->set_max_incoming_bw(out_bw_);
}

void
pisces_cut_through_arbitrator::do_arbitrate(pkt_arbitration_t &st)
{
  pisces_payload* payload = st.pkt;
  payload->init_bw(out_bw_);
  double payload_bw = payload->bw() * bw_sec_to_tick_conversion_;
#if SSTMAC_SANITY_CHECK
  validate_bw(payload->bw());
#endif

  //first things first - clean out any old epochs
  ticks_t now = st.now.ticks_int64();
  clean_up(now);

  ticks_t send_start = head_->start;

  long bytes_queued = payload_bw * (send_start - payload->arrival().ticks_int64());
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
                            payload->to_string().c_str(), st.now.sec(), payload->arrival().sec());

  bandwidth_epoch* epoch = head_;
  while (1) {
    pflow_arb_debug_printf_l1("epoch BW=%9.5e start=%9.5e length=%9.5e: bytes_to_send=%d bytes_queued=%d",
                           epoch->bw_available,
                           epoch->start,
                           epoch->length,
                           bytes_to_send,
                           bytes_queued);

    double delta_bw = payload_bw - epoch->bw_available;

    /**
        This is basically assuming payload->bw >= bw_available
        We use the -1e-6 to avoid huge numbers in the else block
        We are maximally utilizing all bw available
    */
    if (delta_bw > -1e-6) {
      //see if we can send all the bytes in this epoch
      ticks_t time_to_send = bytes_to_send / epoch->bw_available;
      pflow_arb_debug_printf_l2("delta=%8.4e, send_time=%lu using all available bandwidth in epoch",
                             delta_bw, time_to_send);
      if (time_to_send < epoch->length) {
        ticks_t payload_stop = epoch->start + time_to_send;
        ticks_t total_send_time = payload_stop - send_start;
        double new_bw = payload->num_bytes() * bw_tick_to_sec_conversion_ / total_send_time;

        payload->set_bw(new_bw);
        epoch->truncate_after(time_to_send);
        pflow_arb_debug_printf_l1("truncate epoch: start=%llu stop=%llu send_time=%llu new_bw=%12.8e",
                                send_start, payload_stop, total_send_time, payload->bw());
        st.head_leaves = timestamp(send_start, timestamp::exact);
        st.tail_leaves = timestamp(payload_stop, timestamp::exact);
        return;
      }
      else if (time_to_send == epoch->length) {
        ticks_t payload_stop = epoch->start + time_to_send;
        ticks_t total_send_time = payload_stop - send_start;
        double new_bw = payload->num_bytes()*bw_tick_to_sec_conversion_ / total_send_time;
        payload->set_bw(new_bw);
        head_ = epoch->next;
        delete epoch;
        pflow_arb_debug_printf_l2("exact fit: start=%llu stop=%llu send_time=%llu new_bw=%12.8e\n",
                                   send_start, payload_stop, total_send_time, payload->bw());
        st.head_leaves = timestamp(send_start, timestamp::exact);
        st.tail_leaves = timestamp(payload_stop, timestamp::exact);
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
      double time_to_send = bytes_to_send / payload_bw;
      pflow_arb_debug_printf_l2("underutilized, No Queue: time_to_send=%llu",
                                time_to_send);

      if (time_to_send <= epoch->length) {
        epoch->split(time_to_send);
        epoch->bw_available -= payload_bw;

        //configure bandwidth
        ticks_t send_done = epoch->start + time_to_send;
        double new_bw = payload->num_bytes() * bw_tick_to_sec_conversion_ / (send_done - send_start);
        payload->set_bw(new_bw);
        pflow_arb_debug_printf_l2("send finishes: start=%llu stop=%llu new_bw=%12.8e",
                               send_start, send_done, payload->bw());
        st.head_leaves = timestamp(send_start, timestamp::exact);
        st.tail_leaves = timestamp(send_done, timestamp::exact);
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
        epoch->bw_available -= payload_bw;
        bytes_to_send -= payload_bw * epoch->length;
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
      ticks_t send_all_time = bytes_to_send / epoch->bw_available;
      ticks_t time_to_intersect = bytes_queued / (-delta_bw);
      ticks_t time_to_send = std::min(epoch->length, std::min(send_all_time,
                                     time_to_intersect));

      pflow_arb_debug_printf_l2("underutilized, but %d bytes queued: delta=%12.8e "
                             "send_all_time=%llu time_to_intersect=%llu time_to_send=%llu",
                             bytes_queued, delta_bw, send_all_time, time_to_intersect, time_to_send);

      if (time_to_send == send_all_time) {
        //and the message completely finishes
        ticks_t send_done = epoch->start + time_to_send;
        double new_bw = payload->num_bytes() * bw_tick_to_sec_conversion_ / (send_done - send_start);
        payload->set_bw(new_bw);
        pflow_arb_debug_printf_l2("send finishes: start=%llu stop=%llu new_bw=%12.8e",
                               send_start, send_done, payload->bw());
        epoch->truncate_after(time_to_send);
        st.head_leaves = timestamp(send_start, timestamp::exact);
        st.tail_leaves = timestamp(send_done, timestamp::exact);
        return;
      }
      else if (time_to_send == epoch->length) {
#if SSTMAC_SANITY_CHECK
        if (epoch->next == 0) {
          //something freaked out numerically
          //time_to_send should never equal the length of the big long, last epoch
          spkt_throw_printf(sprockit::illformed_error,
                           "Time to send pisces is way too long:\n"
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
        bytes_queued += ceil(delta_bw * epoch->length);
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