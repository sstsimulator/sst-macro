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
class timestamp
{
 public:
  /// The type that holds a timestamp.
  typedef int64_t tick_t;

  static tick_t PSEC_PER_TICK;
  static const timestamp::tick_t zero = 0;
  static timestamp::tick_t nanoseconds;
  static timestamp::tick_t microseconds;
  static timestamp::tick_t milliseconds;
  static timestamp::tick_t seconds;
  static timestamp::tick_t minutes;

 private:
  /// Picoseconds between clock ticks.

  static timestamp
  scaled_time(int64_t value, int64_t scaling, const char *caller,
              const char *units);

  /// The current time value of this container in ticks.
  tick_t ticks_;

 public:
  static void
  init_stamps(tick_t tick_spacing);

  typedef enum { exact } timestamp_param_type_t;

  /// Round the given time value (in seconds) to the nearest
  /// representable internal time.
  /// \throw sprockit::time_error if this time is outside the range that can
  /// can be represented with this time container
  timestamp(double t){
    ticks_ = int64_t((t) * ( 1e12 / (PSEC_PER_TICK)));
    if (t > max_time() || (t < min_time())) {
      spkt_abort_printf("timestamp(): Time value %e out of bounds %e...%e",
                        t, min_time(), max_time());
    }
  }

  explicit timestamp(tick_t ticks, timestamp_param_type_t ty) : ticks_(ticks) {}

  explicit timestamp(uint32_t sec, uint32_t nsec) :
      ticks_(sec*seconds + nsec*nanoseconds) {}

  explicit timestamp() : ticks_(0) {}

  /// Convert a tick type to int64_t.
  int64_t
  ticks_int64() const {
    return ticks_;
  }

  /// Return the current time in seconds.
  double
  sec() const;

  /// Return the current time in milliseconds.
  double
  msec() const;

  /// Return the current time in microseconds.
  double
  usec() const;

  /// Return the current time in nanoseconds.
  double
  nsec() const;

  /// Return the current time in picoseconds.
  double
  psec() const;

  void correct_round_off(const timestamp& now);

 public:
  /// Get the number of ticks.
  inline tick_t
  ticks() const {
    return ticks_;
  }

  /// Get the tick interval in picoseconds.
  static tick_t
  tick_interval();

  /// Get the tick interval in std::string form (for example, "1ps").
  static const std::string &
  tick_interval_string();

  /// Get the number of ticks per second (1e12/tick_interval()).
  static tick_t
  frequency();

  /// Get the largest time value possible (in seconds).
  static double
  max_time();

  /// Get the smallest (most negative) time value possible (in seconds).
  static double
  min_time();

  /// Get a time value corresponding exactly to the given number of
  /// picoseconds.
  /// \throw sprockit::time_error if this time cannot be exactly represented.
  static timestamp
  exact_psec(int64_t psec);

  /// Get a time value corresponding exactly to the given number of
  /// nanoseconds.
  /// \throw sprockit::time_error if this time cannot be exactly represented.
  static timestamp
  exact_nsec(int64_t nsec);

  /// Get a time value corresponding exactly to the given number of
  /// microseconds.
  /// \throw sprockit::time_error if this time cannot be exactly represented.
  static timestamp
  exact_usec(int64_t usec);

  /// Get a time value corresponding exactly to the given number of
  /// milliseconds.
  /// \throw sprockit::time_error if this time cannot be exactly represented.
  static timestamp
  exact_msec(int64_t msec);

  /// Get a time value corresponding exactly to the given number of
  /// seconds.
  /// \throw sprockit::time_error if this time cannot be exactly represented.
  static timestamp
  exact_sec(int64_t sec);

  /// Get a time value with exactly the given number of ticks.
  /// This is a template function to ensure that we do proper range checking
  /// on input values.
  template<typename T>
  static timestamp
  exact_ticks(T val) {
    timestamp ts(0);
    ts.ticks_ = tick_t(val);
    return ts;
  }

  /// Fast and exact comparison operations.
  inline bool
  operator==(const timestamp &other) const {
    return (ticks_ == other.ticks_);
  }
  inline bool
  operator!=(const timestamp &other) const {
    return (ticks_ != other.ticks_);
  }
  inline bool
  operator<(const timestamp &other) const {
    return (ticks_ < other.ticks_);
  }
  inline bool
  operator<=(const timestamp &other) const {
    return (ticks_ <= other.ticks_);
  }
  inline bool
  operator>(const timestamp &other) const {
    return (ticks_ > other.ticks_);
  }
  inline bool
  operator>=(const timestamp &other) const {
    return (ticks_ >= other.ticks_);
  }
  timestamp&
  operator+=(const timestamp &other);
  timestamp&
  operator-=(const timestamp &other);
  timestamp&
  operator*=(double scale);
  timestamp&
  operator/=(double scale);
};

timestamp
operator+(const timestamp &a, const timestamp &b);
timestamp
operator-(const timestamp &a, const timestamp &b);
timestamp
operator*(const timestamp &t, double scaling);
timestamp
operator*(double scaling, const timestamp &t);
timestamp
operator/(const timestamp &t, double scaling);

std::ostream&
operator<<(std::ostream &os, const timestamp &t);

std::string
to_printf_type(timestamp t);


} // end of namespace sstmac

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sstmac::timestamp>
{
 public:
  void
  operator()(sstmac::timestamp& t, serializer& ser){
    ser.primitive(t);
  }
};
END_SERIALIZATION_NAMESPACE

#endif