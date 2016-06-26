/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_COMMON_TIMESTAMP_H_INCLUDED
#define SSTMAC_COMMON_TIMESTAMP_H_INCLUDED

#include <iosfwd>
#include <stdint.h>
#include <iostream>
#include <sstmac/common/serializable.h>

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
  timestamp(double t);

  explicit
  timestamp(tick_t ticks, timestamp_param_type_t ty);

  explicit
  timestamp();

  static tick_t zero;

  /// Convert a tick type to int64_t.
  int64_t
  ticks_int64() const;

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

  static double
  tick_interval_sec();

  /// Get the tick interval in picoseconds.
  static tick_t
  tick_interval();

  static int64_t
  tick_interval_int64();

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

