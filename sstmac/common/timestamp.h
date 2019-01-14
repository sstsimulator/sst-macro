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
  /// The type that holds a timestamp.
  typedef uint64_t tick_t;

  static tick_t PSEC_PER_TICK;
  static const Timestamp::tick_t zero = 0;
  static Timestamp::tick_t nanoseconds;
  static Timestamp::tick_t microseconds;
  static Timestamp::tick_t milliseconds;
  static Timestamp::tick_t seconds;
  static Timestamp::tick_t minutes;

 private:
  tick_t ticks_;
  static double max_time_;
  static double min_time_;
  static double ticks_per_second_;
  static double seconds_per_tick_;
  static double msec_per_tick_;
  static double usec_per_tick_;
  static double nsec_per_tick_;
  static double psec_per_tick_;

 public:
  static void initStamps(tick_t tick_spacing);

  typedef enum { exact } timestamp_param_type_t;

  Timestamp(double t_seconds){
    ticks_ = uint64_t(t_seconds * ticks_per_second_); // * ( 1e12 / (PSEC_PER_TICK)));
    if (t_seconds > maxTime() || (t_seconds < minTime())) {
      spkt_abort_printf("timestamp(): Time value %e out of bounds %e...%e",
                        t_seconds, minTime(), maxTime());
    }
  }

  explicit Timestamp(tick_t ticks, timestamp_param_type_t ty) : ticks_(ticks) {}

  explicit Timestamp(uint64_t num_units, tick_t ticks_per_unit) : ticks_(num_units*ticks_per_unit) {}

  explicit Timestamp() : ticks_(0) {}

  uint64_t ticks_int64() const {
    return ticks_;
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

  void correctRoundOff(const Timestamp& now);

 public:
  inline tick_t ticks() const {
    return ticks_;
  }

  static tick_t tickInterval();

  static const std::string & tickIntervalString();

  static tick_t frequency();

  static double maxTime(){
    return max_time_;
  }

  static double minTime(){
    return min_time_;
  }

  /// Get a time value with exactly the given number of ticks.
  /// This is a template function to ensure that we do proper range checking
  /// on input values.
  template<typename T>
  static Timestamp exact_ticks(T val) {
    Timestamp ts;
    ts.ticks_ = tick_t(val);
    return ts;
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

Timestamp operator+(const Timestamp &a, const Timestamp &b);
Timestamp operator-(const Timestamp &a, const Timestamp &b);
Timestamp operator*(const Timestamp &t, double scaling);
Timestamp operator*(double scaling, const Timestamp &t);
Timestamp operator/(const Timestamp &t, double scaling);

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
