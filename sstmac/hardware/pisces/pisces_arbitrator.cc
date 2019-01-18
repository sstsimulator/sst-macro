/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#if 1
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
#else
#define pflow_arb_debug_printf_l0(format_str, ...)
#define pflow_arb_debug_printf_l1(format_str, ...)
#define pflow_arb_debug_printf_l2(format_str, ...)
#define pflow_arb_debug_print_l2(format_str)
#endif

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

PiscesBandwidthArbitrator::
PiscesBandwidthArbitrator(SST::Params& params)
{
  double bw = params->get_bandwidth_param("bandwidth");
  byteDelay_ = Timestamp(1.0/bw);
}

PiscesSimpleArbitrator::PiscesSimpleArbitrator(SST::Params& params) :
  next_free_(),
  PiscesBandwidthArbitrator(params)
{
}

void
PiscesSimpleArbitrator::arbitrate(pkt_arbitration_t &st)
{
  GlobalTimestamp start_send = next_free_ < st.now ? st.now : next_free_;
  Timestamp arrive_delay = st.pkt->byteLength() * st.pkt->byteDelay();
  Timestamp output_delay = st.pkt->byteLength() * byteDelay_;
  next_free_ = start_send + output_delay;
  st.pkt->initByteDelay(byteDelay_);
  Timestamp creditDelay = output_delay > arrive_delay
        ? output_delay - arrive_delay //if going out slower, delay credit
        : Timestamp();

  //store and forward
  //head/tail are linked and go "at same time"
  st.head_leaves = st.tail_leaves = next_free_;
  //we can send the credit a bit ahead of time
  st.credit_leaves = st.head_leaves + creditDelay;
  st.pkt->setByteDelay(byteDelay_);
}

uint32_t
PiscesSimpleArbitrator::bytesSending(GlobalTimestamp now) const
{
  Timestamp send_delay = next_free_ > now ? (next_free_ - now) : Timestamp();
  return send_delay / byteDelay_;
}

PiscesNullArbitrator::PiscesNullArbitrator(SST::Params& params) :
  PiscesBandwidthArbitrator(params)
{
}

Timestamp
PiscesNullArbitrator::headTailDelay(PiscesPacket *pkt)
{
  return pkt->minByteDelay();
}

void
PiscesNullArbitrator::arbitrate(pkt_arbitration_t &st)
{
  Timestamp byteDelay = std::max(byteDelay_, st.pkt->minByteDelay());
  st.pkt->setByteDelay(byteDelay_);
  Timestamp actual_delay = st.pkt->numBytes() * byteDelay;
  Timestamp min_delay = st.pkt->numBytes() * byteDelay_;
#if SSTMAC_SANITY_CHECK
  if (actual_delay < min_delay){
    spkt_abort_printf("null arbitrator computed bad delay");
  }
#endif
  st.head_leaves = st.now;
  st.tail_leaves = st.now + actual_delay;
  //we can send the credit a bit ahead of the tail
  st.credit_leaves = st.head_leaves + (actual_delay - min_delay);
  st.pkt->setByteDelay(byteDelay_);
}

uint32_t
PiscesNullArbitrator::bytesSending(GlobalTimestamp now) const
{
  return 0;
}

PiscesCutThroughArbitrator::
PiscesCutThroughArbitrator(SST::Params& params)
  : head_(nullptr),
    PiscesBandwidthArbitrator(params)
{
  init_epoch_length_ = Timestamp(params->get_optional_time_param("epoch_length", 1e-6));
  init_num_cycles_ = init_epoch_length_ / byteDelay_;
  //in case of remainders
  init_epoch_length_ = init_num_cycles_ * byteDelay_;
  head_ = BandwidthEpoch::allocate_at_beginning();
  head_->start = GlobalTimestamp();
  head_->numCycles = init_num_cycles_;
  head_->cycleLength = byteDelay_;
  head_->length = init_epoch_length_;
  head_->next = nullptr;
}


Timestamp
PiscesCutThroughArbitrator::headTailDelay(PiscesPacket *pkt)
{
  return pkt->numBytes() * pkt->byteDelay();
}

PiscesCutThroughArbitrator::~PiscesCutThroughArbitrator()
{
  BandwidthEpoch* next = head_;
  while (next){
    BandwidthEpoch* e = next;
    next = next->next;
    //do not delete this for now, treat as permanent
    //this guy gets deleted and created before anything is running
    BandwidthEpoch::free_at_end(e);
  }
}

uint32_t
PiscesCutThroughArbitrator::bytesSending(GlobalTimestamp now) const
{
  Timestamp send_delay = head_->start > now ? (head_->start - now) : Timestamp();
  uint32_t bytes_sending = send_delay / byteDelay_;
  return bytes_sending;
}


PiscesCutThroughArbitrator::BandwidthEpoch*
PiscesCutThroughArbitrator::addEpoch(GlobalTimestamp start, BandwidthEpoch* prev)
{
  BandwidthEpoch* epoch = new BandwidthEpoch;
  epoch->start = start;
  epoch->numCycles = init_num_cycles_;
  epoch->cycleLength = byteDelay_;
  epoch->length = init_epoch_length_;
  epoch->next = nullptr;
  if (!head_){
    head_ = epoch;
  } else if (prev) {
    prev->next = epoch;
  }
  return epoch;
}

PiscesCutThroughArbitrator::BandwidthEpoch*
PiscesCutThroughArbitrator::advance(BandwidthEpoch* epoch)
{
  auto old = epoch;
  if (old == head_){
    head_ = head_->next;
  }
  epoch = epoch->next;
  delete old;
  return epoch;
}

void
PiscesCutThroughArbitrator::arbitrate(pkt_arbitration_t &st)
{
  PiscesPacket* payload = st.pkt;
  payload->initByteDelay(byteDelay_);

  BandwidthEpoch* epoch = head_;
  while(epoch){
    GlobalTimestamp epochEnd = epoch->start + epoch->cycleLength * epoch->numCycles;
    if (epochEnd <= st.now){
      auto old = epoch;
      head_ = epoch->next;
      epoch = epoch->next;
      delete old;
    } else {
      Timestamp delta = epochEnd - st.now;
      uint32_t lostCycles = delta / epoch->cycleLength;
      epoch->start = st.now;
      epoch->numCycles -= lostCycles;
    }
  }

  if (!epoch){
    //we ran out of epochs more
    epoch = addEpoch(st.now, nullptr);
  }

  st.head_leaves = epoch->start;

  Timestamp minTimeToSend = payload->byteDelay() * payload->numBytes();
  Timestamp timeToSend;
  uint32_t bytesLeft = payload->byteLength();
  uint32_t bytesArrived = 0;

  BandwidthEpoch* prev = nullptr;
  while (bytesLeft > 0){

    if (!epoch){
      //we ran out of epochs - add a few more
      epoch = addEpoch(st.now + timeToSend, prev);
    }

    if (bytesArrived != 0){
      //adjust the payload arrival rate
      //we have bytes queueing up - simulate this as bytes arriving "faster"
      Timestamp newByteDelay = (payload->byteDelay() * (payload->byteLength() - bytesArrived)) / payload->byteLength();
      payload->setByteDelay(newByteDelay);
      bytesArrived = 0;
    }

    if (epoch->cycleLength > payload->byteDelay()){
      //this epoch is sending slower than the packet is arriving
      if (epoch->numCycles > bytesLeft){
        Timestamp timeIncrement = bytesLeft * epoch->cycleLength;
        timeToSend += timeIncrement;
        epoch->numCycles -= bytesLeft;
        epoch->start += timeIncrement;
        bytesLeft = 0;
        pflow_arb_debug_printf_l0("On epoch %u:%9.5e sent ALL %u bytes of packet with cycles left",
                epoch->numCycles + bytesLeft, epoch->cycleLength.sec(), bytesLeft);
      } else if (epoch->numCycles == bytesLeft){
        //the previous epoch is exactly used up
        pflow_arb_debug_printf_l0("On epoch %u:%9.5e sent EXACTLY %u bytes of packet",
                epoch->numCycles + bytesLeft, epoch->cycleLength.sec(), bytesLeft);
        timeToSend += bytesLeft * epoch->cycleLength;
        bytesLeft = 0;
        epoch = advance(epoch);
      } else {
        //the previous epoch is more than used up
        timeToSend += epoch->numCycles * epoch->cycleLength;
        bytesLeft -= epoch->numCycles;
        pflow_arb_debug_printf_l0("On epoch %u:%llu sent all cycles used, %u bytes left",
                epoch->numCycles, epoch->cycleLength.ticks(), bytesLeft);
        uint32_t bytesPossible = payload->byteDelay() * epoch->numCycles / epoch->cycleLength;
        bytesArrived = bytesPossible - epoch->numCycles;
        bytesArrived = std::min(bytesLeft, bytesArrived);
        epoch = advance(epoch);
      }
    } else {
      //the epoch is sending faster than packet arriving
      uint32_t bytesPossible = (epoch->numCycles * epoch->cycleLength) / payload->byteDelay();
      uint32_t bytesSent = std::min(bytesPossible, bytesLeft);
      Timestamp timeIncrement = bytesLeft * payload->byteDelay();
      timeToSend += timeIncrement;
      bytesLeft -= bytesSent;
      if (bytesSent == epoch->numCycles){ //huh, okay, epoch used up
        pflow_arb_debug_printf_l0("On epoch %u:%llu exactly sent, %u bytes left",
                epoch->numCycles, epoch->cycleLength.ticks(), bytesSent, bytesLeft);
        epoch = advance(epoch);
      } else { //subset of epoch left
        auto old = epoch->cycleLength;
        epoch->cycleLength = epoch->cycleLength * epoch->numCycles / (epoch->numCycles - bytesSent);
        if (old > epoch->cycleLength) abort();
        epoch->numCycles -= bytesLeft;
        pflow_arb_debug_printf_l0("On epoch %u:%llu sent subset %u of cycles, %u bytes left",
                epoch->numCycles, epoch->cycleLength.ticks(), bytesSent, bytesLeft);
      }
    }
  }

  Timestamp creditDelay = timeToSend - minTimeToSend;
  st.credit_leaves = st.head_leaves + creditDelay;
  st.tail_leaves = st.head_leaves + timeToSend;
#if SSTMAC_SANITY_CHECK
  if (st.credit_leaves < st.now){
    spkt_abort_printf("pisces arbitrator computed bad credit departure");
  }
#endif

  Timestamp newByteDelay = timeToSend / st.pkt->numBytes();
  st.pkt->setByteDelay(newByteDelay);

  if (!head_){ //hmm, all gone
    addEpoch(st.tail_leaves, nullptr);
  }
}

}
}
