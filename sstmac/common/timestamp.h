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

#ifndef SSTMAC_COMMON_TIMESTAMP_H_INCLUDED
#define SSTMAC_COMMON_TIMESTAMP_H_INCLUDED

#include <iosfwd>
#include <stdint.h>
#include <iostream>
#include <sstmac/common/serializable.h>
#include <sprockit/errors.h>

namespace sstmac {

/**
 * A basic container for time (subject to future transplant).
 * Stores time as an integral number of picoseconds (tentatively).
 * With 1 psec resolution, a 64 bit int can hold roughly +/- 106 days.
 *
 * Intended to be reasonably compatible with ns3::HighPrecision time.
 */
class Timestamp
{
 public:
  friend class GlobalTimestamp;

  /// The type that holds a timestamp.
  typedef uint64_t tick_t;

  static tick_t ASEC_PER_TICK;
  static const tick_t zero = 0;
  static tick_t one_femtosecond;
  static tick_t one_picosecond;
  static tick_t one_nanosecond;
  static tick_t one_microsecond;
  static tick_t one_millisecond;
  static tick_t one_second;
  static tick_t one_minute;

  static double s_per_tick;
  static double ms_per_tick;
  static double us_per_tick;
  static double ns_per_tick;
  static double ps_per_tick;
  static double fs_per_tick;

 private:
  tick_t ticks_;
  static double max_time_;

 public:
  static void initStamps(tick_t tick_spacing);

  typedef enum { exact } timestamp_param_type_t;

  explicit Timestamp(double t_seconds){
    ticks_ = uint64_t(t_seconds * one_second);
    if (t_seconds > maxTime()) {
      spkt_abort_printf("Timestamp(): Time value %e out of bounds 0...%e",
                        t_seconds, maxTime());
    }
  }

  explicit Timestamp(tick_t ticks, timestamp_param_type_t ty) : ticks_(ticks) {}

  explicit Timestamp(uint64_t num_units, tick_t ticks_per_unit) : ticks_(num_units*ticks_per_unit) {}

  explicit Timestamp() : ticks_(0) {}

  static uint32_t divideUp(Timestamp num, Timestamp denom){
    //optimize for architectures that generate remainder bit
    //this should optimize
    uint32_t x = num.ticks();
    uint32_t y = denom.ticks();
    uint32_t xy = x/y;
    uint32_t res = (x % y) ? xy + 1 : xy;
    return res;
  }

#if ACTUAL_INTEGRATED_SST_CORE
  operator SST::SimTime_t() const {
    return ticks_;
  }
#endif

  double sec() const;

  double msec() const;

  double usec() const;

  double nsec() const;

  double psec() const;

 public:
  inline tick_t ticks() const {
    return ticks_;
  }

  static tick_t tickInterval();

  static const std::string & tickIntervalString();

  static double maxTime(){
    return max_time_;
  }

  inline bool operator==(const Timestamp &other) const {
    return (ticks_ == other.ticks_);
  }

  inline bool operator!=(const Timestamp &other) const {
    return (ticks_ != other.ticks_);
  }

  inline bool operator<(const Timestamp &other) const {
    return (ticks_ < other.ticks_);
  }

  inline bool operator<=(const Timestamp &other) const {
    return (ticks_ <= other.ticks_);
  }

  inline bool operator>(const Timestamp &other) const {
    return (ticks_ > other.ticks_);
  }

  inline bool operator>=(const Timestamp &other) const {
    return (ticks_ >= other.ticks_);
  }

  Timestamp& operator+=(const Timestamp &other);
  Timestamp& operator-=(const Timestamp &other);
  Timestamp& operator*=(double scale);
  Timestamp& operator/=(double scale);
};

struct GlobalTimestamp
{
  friend class Timestamp;

  explicit GlobalTimestamp() : epochs(0), time()
  {
  }

  explicit GlobalTimestamp(uint64_t eps, Timestamp tcks) :
    epochs(eps), time(tcks)
  {
  }

  explicit GlobalTimestamp(uint64_t eps, uint64_t subticks) :
    epochs(eps), time(subticks, Timestamp::exact)
  {
  }

  explicit GlobalTimestamp(double t) :
    epochs(0), time(t)
  {
  }

  double sec() const {
    return time.sec();
  }

  double usec() const {
    return time.usec();
  }

  double nsec() const {
    return time.nsec();
  }

  uint64_t usecRounded() const {
    return time.ticks() / time.one_microsecond;
  }

  GlobalTimestamp& operator+=(const Timestamp& t);

  uint64_t epochs;
  Timestamp time;

  static constexpr uint64_t carry_bits_mask = 0;
  static constexpr uint64_t remainder_bits_mask = ~uint64_t(0);
  static constexpr uint64_t carry_bits_shift = 0;
};

Timestamp operator+(const Timestamp &a, const Timestamp &b);
Timestamp operator-(const Timestamp &a, const Timestamp &b);
Timestamp operator*(const Timestamp &t, double scaling);
Timestamp operator*(double scaling, const Timestamp &t);
Timestamp operator/(const Timestamp &t, double scaling);
static inline uint64_t operator/(const Timestamp& a, const Timestamp& b){
  return a.ticks() / b.ticks();
}

static inline GlobalTimestamp operator+(const GlobalTimestamp& a, const Timestamp& b)
{
  uint64_t sum = a.time.ticks() + b.ticks();
  uint64_t carry = (sum & GlobalTimestamp::carry_bits_mask) << GlobalTimestamp::carry_bits_shift;
  uint64_t rem = sum & GlobalTimestamp::remainder_bits_mask;
  return GlobalTimestamp(carry + a.epochs, Timestamp(rem, Timestamp::exact));
}

static inline Timestamp operator-(const GlobalTimestamp& a, const GlobalTimestamp& b){
  if (a.epochs == b.epochs){
    return a.time - b.time;
  } else {
    ::abort(); //TODO
  }
}

static inline GlobalTimestamp operator+(const Timestamp& a, const GlobalTimestamp& b){
  return b + a;
}

static inline GlobalTimestamp operator+(const GlobalTimestamp& a, const GlobalTimestamp& b){
  GlobalTimestamp tmp = a.time + b;
  tmp.epochs += a.epochs;
  return tmp;
}

static inline GlobalTimestamp operator-(const GlobalTimestamp& a, const Timestamp b){
  if (a.time > b){
    GlobalTimestamp tmp = a;
    tmp.time -= b;
    return tmp;
  } else {
    uint64_t ticks = GlobalTimestamp::remainder_bits_mask - b.ticks();
    GlobalTimestamp gt(a.epochs-1, Timestamp(ticks, Timestamp::exact));
    return gt;
  }
}

static inline bool operator>=(const GlobalTimestamp& a, const GlobalTimestamp& b){
  if (a.epochs == b.epochs){
    return a.time >= b.time;
  } else {
    return a.epochs >= b.epochs;
  }
}

static inline bool operator!=(const GlobalTimestamp& a, const GlobalTimestamp& b){
  if (a.epochs == b.epochs){
    return a.time != b.time;
  } else {
    return false;
  }
}

static inline bool operator<(const GlobalTimestamp& a, const GlobalTimestamp& b){
  if (a.epochs == b.epochs){
    return a.time < b.time;
  } else {
    return a.epochs < b.epochs;
  }
}

static inline bool operator==(const GlobalTimestamp& a, const GlobalTimestamp& b){
  return a.epochs == b.epochs && a.time == b.time;
}

static inline bool operator>(const GlobalTimestamp& a, const GlobalTimestamp& b){
  return b < a;
}

static inline bool operator<=(const GlobalTimestamp& a, const GlobalTimestamp& b){
  if (a.epochs == b.epochs){
    return a.time <= b.time;
  } else {
    return a.time < b.time;
  }
}

std::ostream& operator<<(std::ostream &os, const Timestamp &t);

std::string to_printf_type(Timestamp t);


} // end of namespace sstmac

START_SERIALIZATION_NAMESPACE
template <> class serialize<sstmac::Timestamp>
{
 public:
  void operator()(sstmac::Timestamp& t, serializer& ser){
    ser.primitive(t);
  }
};
END_SERIALIZATION_NAMESPACE

#endif
