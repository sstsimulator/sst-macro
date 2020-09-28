/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
#include <unusedvariablemacro.h>

#include <math.h>

#define one_indent "  "
#define two_indent "    "

#if PISCES_DEBUG_INDIVIDUAL_HISTORY
#define dprintf(flag, ...) \
  history_.push_back(sprockit::sprintf(__VA_ARGS__))
#else
#define dprintf(flag, ...) debug_printf(flag, __VA_ARGS__)
#endif

#if PISCES_DETAILED_DEBUG
#define pflow_arb_debug_printf_l0(format_str, ...) \
  dprintf(sprockit::dbg::pisces,  \
    " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_printf_l1(format_str, ...) \
  dprintf(sprockit::dbg::pisces,  \
    one_indent " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_printf_l2(format_str, ...) \
  dprintf(sprockit::dbg::pisces,  \
    two_indent " [arbitrator] " format_str , \
    __VA_ARGS__)

#define pflow_arb_debug_print_l2(format_str) \
  dprintf(sprockit::dbg::pisces,  \
    two_indent " [arbitrator] " format_str "%s", "")
#else
#define pflow_arb_debug_printf_l0(format_str, ...)
#define pflow_arb_debug_printf_l1(format_str, ...)
#define pflow_arb_debug_printf_l2(format_str, ...)
#define pflow_arb_debug_print_l2(format_str)
#endif

#define cut_through_epoch_debug(str, ...) \
  pflow_arb_debug_printf_l0("Cut-through: arbitrator %p epoch %9.5e:%llu " str, \
        this, epoch->start.sec(), epoch->numCycles, __VA_ARGS__)

#define cut_through_arb_debug(str, ...) \
  cut_through_epoch_debug(str ": %u bytes left", __VA_ARGS__, bytesLeft)

#define cut_through_arb_debug_noargs(str) \
  cut_through_arb_debug(str "%s", "")


namespace sstmac {
namespace hw {

PiscesBandwidthArbitrator::
PiscesBandwidthArbitrator(double bw)
{
  byteDelay_ = TimeDelta(1.0/bw);
}

PiscesSimpleArbitrator::PiscesSimpleArbitrator(double bw) :
  PiscesBandwidthArbitrator(bw),
  next_free_()
{
}

void
PiscesSimpleArbitrator::arbitrate(IncomingPacket &st)
{
  Timestamp start_send = next_free_ < st.now ? st.now : next_free_;
  TimeDelta output_delay = st.pkt->byteLength() * byteDelay_;
  next_free_ = start_send + output_delay;
  st.pkt->initByteDelay(byteDelay_);

  //store and forward
  //head/tail are linked and go "at same time"
  st.head_leaves = st.tail_leaves = next_free_;
  //head and tail arrive at same time - there is no delay
  //for additional bytes to arrive
  st.pkt->setByteDelay(TimeDelta());
}

PiscesNullArbitrator::PiscesNullArbitrator(double bw) :
  PiscesBandwidthArbitrator(bw)
{
}

TimeDelta
PiscesNullArbitrator::headTailDelay(PiscesPacket *pkt)
{
  return pkt->numBytes() * pkt->byteDelay();
}

void
PiscesNullArbitrator::arbitrate(IncomingPacket &st)
{
  SSTMAC_MAYBE_UNUSED PiscesPacket* payload = st.pkt;
  pflow_arb_debug_printf_l0("Null: starting packet %p:%llu of size %u with byte_delay=%9.5e epoch_delay=%9.5e: %s",
                            payload, payload->flowId(),
                            payload->numBytes(), payload->byteDelay().sec(), byteDelay_.sec());
  TimeDelta byteDelay = std::max(byteDelay_, st.pkt->byteDelay());
  st.pkt->setByteDelay(byteDelay_);
  TimeDelta actual_delay = st.pkt->numBytes() * byteDelay;
#if SSTMAC_SANITY_CHECK
  TimeDelta min_delay = st.pkt->numBytes() * byteDelay_;
  if (actual_delay < min_delay){
    spkt_abort_printf("null arbitrator computed bad delay");
  }
#endif
  st.head_leaves = st.now;
  st.tail_leaves = st.now + actual_delay;
  st.pkt->setByteDelay(byteDelay);

  pflow_arb_debug_printf_l0("Null: packet %p:%llu of size %u leaves with effective bandwidth=%9.5e with epoch max=%9.5e",
          payload, payload->flowId(), payload->numBytes(), 1.0/byteDelay.sec(), 1.0/byteDelay_.sec());
}

PiscesCutThroughArbitrator::
PiscesCutThroughArbitrator(double bw)
  : PiscesBandwidthArbitrator(bw),
    head_(nullptr)
{
  cycleLength_ = byteDelay_;
  head_ = Epoch::allocateAtBeginning();
  head_->numCycles = std::numeric_limits<uint32_t>::max();
}


TimeDelta
PiscesCutThroughArbitrator::headTailDelay(PiscesPacket *pkt)
{
  return pkt->numBytes() * pkt->byteDelay();
}

PiscesCutThroughArbitrator::~PiscesCutThroughArbitrator()
{
  auto* next = head_;
  while (next) {
    auto* toDel = next;
    next = next->next;
    Epoch::freeAtEnd(toDel);
  }
}

void
PiscesCutThroughArbitrator::clearOut(Timestamp now)
{
  Epoch* epoch = head_;
  while (epoch){
    cut_through_epoch_debug("clearing at %9.5e", now.sec());
    Timestamp end = epoch->start + epoch->numCycles * cycleLength_;
    if (now <= epoch->start){
      if (!epoch->next){
        //this is the last epoch, restore it to full size
        epoch->numCycles = std::numeric_limits<uint32_t>::max();
      }
      return;
    } else if (now < end){
      if (epoch->next){
        TimeDelta lostTime = now - epoch->start;
        uint32_t lostCycles = lostTime / cycleLength_;
        epoch->numCycles -= lostCycles;
      } else {
        //this is the last epoch - restore it to "full size"
        epoch->numCycles = std::numeric_limits<uint32_t>::max();
      }
      epoch->start = now;
      return;
    } else {
      if (epoch->next){
        Epoch* next = epoch->next;
        delete epoch;
        head_ = next;
#if SSTMAC_SANITY_CHECK
        if (head_ == nullptr){
#if PISCES_DEBUG_INDIVIDUAL_HISTORY
          for (auto& str : history_){
            std::cerr << str << std::endl;
          }
#endif
          spkt_abort_printf("internal error: head epoch is null in PiscesCutThroughArbitrator");
        }
#endif
        epoch = next;
      } else {
        //this is the last epoch - restore it to "full size"
        epoch->numCycles = std::numeric_limits<uint32_t>::max();
        epoch->start = now;
        return;
      }
    }
  }
}

PiscesCutThroughArbitrator::Epoch*
PiscesCutThroughArbitrator::advance(Epoch* epoch, Epoch* prev)
{
  Epoch* next = epoch->next;
  if (prev) prev->next = epoch->next;
  else head_ = next;
  delete epoch;
#if SSTMAC_SANITY_CHECK
  if (head_ == nullptr){
  #if PISCES_DEBUG_INDIVIDUAL_HISTORY
    for (auto& str : history_){
      std::cerr << str << std::endl;
    }
  #endif
    spkt_abort_printf("internal error: head epoch is null in PiscesCutThroughArbitrator");
  }
#endif
  return next;
}

void
PiscesCutThroughArbitrator::arbitrate(IncomingPacket &st)
{
  pflow_arb_debug_printf_l0("Cut-through: arbitrator %p starting packet %p:%llu of size %u with byte_delay=%9.5e epoch_delay=%9.5e start=%9.5e: %s",
                          this, st.pkt, st.pkt->flowId(), st.pkt->numBytes(), st.pkt->byteDelay().sec(),
                          byteDelay_.sec(), st.now.sec(),
                          (st.pkt->flow() ? sprockit::toString(st.pkt->flow()).c_str() : "null payload"));

#define PRINT_EPOCHS 0
#if PRINT_EPOCHS
  std::cout << "------" << std::endl;
  Epoch* start_ep = head_;
  while(start_ep){
    std::cout << "Start epoch " << this << ": " << start_ep->start.time.ticks() << ": " << start_ep->numCycles << std::endl;
    start_ep = start_ep->next;
  }
  std::cout << "------" << std::endl;
#endif

  clearOut(st.now);

  Timestamp fullyBufferedTime = st.now + st.pkt->byteDelay() * st.pkt->numBytes();
  Epoch* epoch = head_;
  Epoch* prev = nullptr;
  uint32_t bytesSent = 0;
  uint32_t bytesLeft = st.pkt->numBytes();
  //first idle epoch
  st.head_leaves = epoch->start;
  while (bytesLeft > 2){ //we often end up with 1,2 byte stragglers - ignore them for efficiency
#if SSTMAC_SANITY_CHECK
    if (!epoch){
#if PISCES_DEBUG_INDIVIDUAL_HISTORY
      for (auto& str : history_){
        std::cerr << str << std::endl;
      }
#endif
      spkt_abort_printf("ran out of epochs on arbitrator %p: this should not be possible", this);
    }
#endif
    TimeDelta epochLength = epoch->numCycles * cycleLength_;
    Timestamp epochEnd = epoch->start + epochLength;
    if (st.pkt->byteDelay() <= cycleLength_){
      //every cycle gets used, arriving faster than leaving
      if (epoch->numCycles <= bytesLeft){
        cut_through_arb_debug_noargs("used all cycles");
        //epoch is completely busy
        bytesSent += epoch->numCycles;
        bytesLeft -= epoch->numCycles;
        epoch = advance(epoch, prev);
        st.tail_leaves = epochEnd;
      } else {
        //epoch has to split into busy and idle halves
        cut_through_arb_debug_noargs("truncating and finishing");
        epoch->start += bytesLeft * cycleLength_;
        epoch->numCycles -= bytesLeft;
        st.tail_leaves = epoch->start;
        bytesLeft = 0;
        bytesSent = st.pkt->numBytes();
      }
    } else {
      //we do not have all the bytes here yet
      if (fullyBufferedTime >= epochEnd){
        uint32_t bytesArrived = (epochEnd - st.now) / st.pkt->byteDelay();
        uint32_t bytesBuffered = std::min(bytesLeft, bytesArrived - bytesSent);
        if (bytesBuffered >= epoch->numCycles){
          cut_through_arb_debug_noargs("buffering finishes after epoch: epoch used up");
          bytesSent += epoch->numCycles;
          bytesLeft -= epoch->numCycles;
          epoch = advance(epoch, prev);
        } else {
          cut_through_arb_debug_noargs("buffering finishes after epoch: epoch has leftover cycles");
          epoch->numCycles -= bytesBuffered;
          prev = epoch;
          epoch = epoch->next;
          bytesSent += bytesBuffered;
          bytesLeft -= bytesBuffered;
        }
        //in case this is the end
        st.tail_leaves = epochEnd;
      } else if (fullyBufferedTime <= epoch->start) {
        cut_through_arb_debug("buffering finishes before epoch: left=%u cycles=%u",
                              bytesLeft, epoch->numCycles);
        //packet is completely buffered
        if (epoch->numCycles > bytesLeft){
          epoch->numCycles -= bytesLeft;
          epoch->start += bytesLeft * cycleLength_;
          st.tail_leaves = epoch->start;
          bytesLeft = 0;
          bytesSent = st.pkt->numBytes();
        } else {
          bytesLeft -= epoch->numCycles;
          bytesSent += epoch->numCycles;
          //epoch is used up
          epoch = advance(epoch, prev);
          st.tail_leaves = epochEnd;
        }
      } else {
        //buffering finishes in the middle of the epoch
        //split the epochs on buffering finishing and repeat
        Epoch* next = new Epoch;
        TimeDelta deltaT = fullyBufferedTime - epoch->start;
        uint32_t preCycles = deltaT  / cycleLength_;
        uint32_t postCycles = epoch->numCycles - preCycles;
        cut_through_arb_debug("buffering finishes during epoch: pre=%u post=%u",
                              preCycles, postCycles);
        epoch->numCycles = preCycles;
        if (epoch->next){
          //there are more epochs after this
          next->next = epoch->next;
          next->numCycles = postCycles;
        } else {
          next->next = nullptr;
          //oh, this is the last epoch - replenish the cycles
          next->numCycles = std::numeric_limits<uint32_t>::max();
        }
        next->start = epoch->start + deltaT;
        epoch->next = next;
      }
    }
  }

  TimeDelta newByteDelay = (st.tail_leaves - st.head_leaves) / st.pkt->numBytes();
  st.pkt->setByteDelay(newByteDelay);

#if SSTMAC_SANITY_CHECK
  Epoch* end_ep = head_;
  Timestamp end_last;
  while(end_ep){
#if PRINT_EPOCHS
    std::cout << "End epoch " << this << ": " << end_ep->start.time.ticks() << ": " << end_ep->numCycles << std::endl;
#endif
    if (end_last > end_ep->start){
      Epoch* test = head_;
      while (test){
        std::cerr << "Epoch " << test->start.time.ticks() << ": " << test->numCycles << std::endl;
        test = test->next;
      }
      spkt_abort_printf("arbitration epochs go backward in time");
    }
    end_last = end_ep->start;
    end_ep = end_ep->next;
  }
#endif

  pflow_arb_debug_printf_l0("Cut-through: arbitrator %p finished packet %p:%llu of size %u with byte_delay=%9.5e epoch_delay=%9.5e head=%9.5e tail=%9.5e",
                          this, st.pkt, st.pkt->flowId(), st.pkt->numBytes(), newByteDelay.sec(), byteDelay_.sec(),
                          st.head_leaves.sec(), st.tail_leaves.sec());

#if SSTMAC_SANITY_CHECK
  if (st.head_leaves > st.tail_leaves){
    spkt_abort_printf("head leaves after tail!");
  }
#endif

}

}
}
