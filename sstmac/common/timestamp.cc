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

#define _ISOC99_SOURCE // llabs was added in C99
#include <sstmac/common/timestamp.h>
#include <sprockit/errors.h>

#include <iostream>
#include <sstream>
#include <iomanip>
#include <limits>
#include <stdio.h>
#include <math.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

namespace sstmac {

// Utility func. to avoid endless ifndef SSTMAC_USE_GMPXX calls.
#ifndef SSTMAC_USE_GMPXX
#define TOD(D) double(D)
#else
inline double TOD(const timestamp::tick_t &t)
{
  return t.get_d();
}
#endif

timestamp::tick_t timestamp::zero = 0;
timestamp::tick_t nanoseconds = 1000;
timestamp::tick_t microseconds = 1000 * nanoseconds;
timestamp::tick_t milliseconds = 1000 * microseconds;
timestamp::tick_t seconds = 1000 * milliseconds; //default is 1 tick per ps
timestamp::tick_t minutes = seconds * 60;

//
// Static variables.
//
static timestamp::tick_t _psec_tick_spacing_ = 1;
#define PSEC_PER_TICK _psec_tick_spacing_

static double _double_to_tick_prefactor_ = 1e12;
static double _tick_to_double_prefactor_ = 1e-12;
static std::string _tick_spacing_string_("1 ps");

//
// Private method to get around the fact that GMP doesn't support
// assignment/construction from long long.
//
inline timestamp::tick_t to_tick(int64_t value)
{
#ifdef SSTMAC_USE_GMPXX
  if(sizeof(int64_t) > sizeof(long)) {
    // Mask with low 31 bits set.
    static const int64_t mask = (uint64_t(1) << 31) - 1;
    const int32_t lowbits = int32_t(value & mask);
    const int32_t highbits = value >> 31;
    timestamp::tick_t retval(highbits);
    retval = retval << 31;
    retval = retval + lowbits;
    return retval;
  }
  else {
    return timestamp::tick_t(long(value));
  }
#else
  return timestamp::tick_t(value);
#endif
}

void timestamp::init_stamps(tick_t tick_spacing)
{
  //psec_tick_spacing_ = new tick_t(tick_spacing);
  PSEC_PER_TICK = tick_spacing;
  _double_to_tick_prefactor_ = 1e12 / TOD(tick_spacing);
  _tick_to_double_prefactor_ = 1e-12 * TOD(tick_spacing);
  std::stringstream ss;
  ss << tick_spacing << " ps";
  //tick_spacing_string_ = new std::string(ss.str());
  _tick_spacing_string_ = ss.str();
  nanoseconds = 1000 / PSEC_PER_TICK;
  microseconds = (1000 * 1000) / PSEC_PER_TICK;
  milliseconds = (1000 * 1000 * 1000) / PSEC_PER_TICK;
  seconds = (tick_t(1000) * 1000 * 1000 * 1000) / PSEC_PER_TICK;
  minutes = 60 * seconds;
}

//
// Round the given time value to the nearest representable internal time.
//
timestamp::timestamp(double t)
{
#ifndef SSTMAC_USE_GMPXX
  ticks_ = int64_t((t) * ( 1e12 / (PSEC_PER_TICK)));
  if (t > max_time() || (t < min_time())) {
    cerrn << "timestamp():  Time value " << t << " is outside the range "
              << max_time() << ".." << min_time() << " available with "
              << PSEC_PER_TICK << " picosecond clock resolution.\n";
    throw sprockit::value_error("timestamp(double):  Time value out of bounds.");
  }
#else
  ticks_ = t * _double_to_tick_prefactor_;
  // ticks_ = t * 1e12;
  //ticks_ /= *psec_tick_spacing_;
#endif
}

timestamp::timestamp()
  : ticks_(0)
{
}

timestamp::timestamp(timestamp::tick_t ticks,
                     timestamp::timestamp_param_type_t t)
  : ticks_(ticks)
{
}

//
// Assignment.
//
//timestamp& timestamp::operator=(double v) {
//  ticks_ = (int64_t((v * 1e12) / *psec_tick_spacing_));
//  return *this;
//}

//
// Return the current time in seconds.
//
double timestamp::sec() const
{
  return TOD(ticks_) * _tick_to_double_prefactor_;
  //return (TOD(ticks_) * 1e-12) * TOD(*psec_tick_spacing_);
}

//
// Return the current time in milliseconds.
//
double timestamp::msec() const
{
  return (TOD(ticks_) * 1e-9) * TOD(PSEC_PER_TICK);
}

//
// Return the current time in microseconds.
//
double timestamp::usec() const
{
  return (TOD(ticks_) * 1e-6) * TOD(PSEC_PER_TICK);
}

//
// Return the current time in nanoseconds.
//
double timestamp::nsec() const
{
  return (TOD(ticks_) * 1e-3) * TOD(PSEC_PER_TICK);
}

//
// Return the current time in picoseconds.
//
double timestamp::psec() const
{
  return TOD(ticks_) * TOD(PSEC_PER_TICK);
}

void
timestamp::correct_round_off(const timestamp &now)
{
#ifndef SSTMAC_USE_GMPXX
  if (ticks_ == (now.ticks_ - 1)) {
    ticks_ = now.ticks_;
  }
#endif
}

//
// static:  Get the tick interval.
//
timestamp::tick_t timestamp::tick_interval()
{
  return PSEC_PER_TICK;
}

double timestamp::tick_interval_sec()
{
  return 1e-12 * TOD(PSEC_PER_TICK);
}

//
// static:  Get the tick interval.
//
int64_t timestamp::tick_interval_int64()
{
#ifndef SSTMAC_USE_GMPXX
  return PSEC_PER_TICK;
#else
  return PSEC_PER_TICK.get_si();
#endif
}

//
// Get the tick interval in std::string form (for example, "1ps").
//
const std::string& timestamp::tick_interval_string()
{
  return _tick_spacing_string_;
}

//
// static:  Convert a tick type to int64_t.
//
int64_t timestamp::ticks_int64() const
{
#ifndef SSTMAC_USE_GMPXX
  return ticks_;
#else
  if(sizeof(long) >= sizeof(int64_t)) {
    return ticks_.get_si();
  }
  else {
    tick_t mask((1 << 31) - 1);
    tick_t lowbits = ticks_ & mask;
    tick_t highbits = ticks_ >> 31;
    int64_t lowraw = lowbits.get_si();
    int64_t highraw = highbits.get_si();
    return ((highraw << 31) | lowraw);
  }
#endif
}

//
// static:  Get the number of ticks per second (1/tick_interval()).
//
timestamp::tick_t timestamp::frequency()
{
  return (tick_t(1e12) / PSEC_PER_TICK);
}

//
// static:  Get the largest time value possible (in seconds).
//
double timestamp::max_time()
{
#ifndef SSTMAC_USE_GMPXX
  return ((std::numeric_limits<int64_t>::max()) * 1e-12) * PSEC_PER_TICK;
#else
  return INFINITY;
#endif
}

//
// static:  Get the smallest (most negative) time value possible (in seconds).
//
double timestamp::min_time()
{
#ifndef SSTMAC_USE_GMPXX
  return (std::numeric_limits<int64_t>::min() * 1e-12) * PSEC_PER_TICK;
#else
  return -INFINITY;
#endif
}

//
// static:  Get a time value corresponding exactly to the given number of
// picoseconds.
//
timestamp timestamp::exact_psec(int64_t psec)
{
  return scaled_time(psec, 1, "timestamp::exact_psec", "picoseconds");
}

//
// static:  Get a time value corresponding exactly to the given number of
// nanoseconds.
//
timestamp timestamp::exact_nsec(int64_t nsec)
{
  return scaled_time(nsec, 1e3, "timestamp::exact_nsec", "nanoseconds");
}

//
// static:  Get a time value corresponding exactly to the given number of
// microseconds.
//
timestamp timestamp::exact_usec(int64_t usec)
{
  return scaled_time(usec, 1e6, "timestamp::exact_usec", "microseconds");
}

//
// static:  Get a time value corresponding exactly to the given number of
// milliseconds.
//
timestamp timestamp::exact_msec(int64_t msec)
{
  return scaled_time(msec, 1e9, "timestamp::exact_msec", "milliseconds");
}

//
// static:: Get a time value corresponding exactly to the given number of
// seconds.
//
timestamp timestamp::exact_sec(int64_t sec)
{
  return scaled_time(sec, 1e12, "timestamp::exact_sec", "seconds");
}

//
// Static:  Get scaled time.
//
timestamp timestamp::scaled_time(int64_t value, int64_t scaling,
                                 const char *caller, const char *units)
{
#ifndef SSTMAC_USE_GMPXX
  static const int64_t max = std::numeric_limits<int64_t>::max();
  static const int64_t min = std::numeric_limits<int64_t>::min();
  if(((value/(PSEC_PER_TICK)) > (max/scaling)) ||
      ((value/(PSEC_PER_TICK)) < (min/scaling))) {
    cerrn << caller << ":  The time value " << value << " " << units
              << " is outside the range of "
              << min << " through " << max << " picoseconds\n";
    throw sprockit::value_error("timestamp::scaled_time:  Value out of range.");
  }
#endif // ! SSTMAC_USE_GMPXX
  timestamp retval(0);
  retval.ticks_ = to_tick(value) * to_tick(scaling) / PSEC_PER_TICK;

  return retval;
}

//
// Add.
//
timestamp& timestamp::operator+=(const timestamp &other)
{
#ifndef SSTMAC_USE_GMPXX
  double approximate = this->sec() + other.sec();
  if(approximate < min_time() || approximate > max_time()) {
    cerrn << "operator+=:  Result " << approximate
              << " is out of range " << min_time() << ".."
              << max_time() << "\n";
    throw sprockit::time_error("timestamp::operator+=():  Result out of range.");
  }
#endif
  ticks_ += other.ticks_;
  return *this;
}

//
// Subtract.
//
timestamp& timestamp::operator-=(const timestamp &other)
{
#ifndef SSTMAC_USE_GMPXX
  double approximate = this->sec() - other.sec();
  if(approximate < min_time() || approximate > max_time()) {
    cerrn << "operator-=:  Result " << approximate
              << " is out of range " << min_time() << ".."
              << max_time() << "\n";
    throw sprockit::time_error("timestamp::operator-=():  Result out of range.");
  }
#endif
  ticks_ -= other.ticks_;
  return *this;
}

//
// Multiply.
//
timestamp& timestamp::operator*=(double scale)
{
#ifndef SSTMAC_USE_GMPXX
  double approximate = this->sec() * scale;
  if(approximate < min_time() || approximate > max_time()) {
    cerrn << "operator*=:  Result " << approximate
              << " is out of range " << min_time() << ".."
              << max_time() << "\n";
    throw sprockit::time_error("timestamp::operator*=():  Result out of range.");
  }
  ticks_ *= scale;
#else
  // gmpxx has stupid promotion rules; using operator*= will
  // promote (truncate) the floating point value to an integer
  // and then perform an integer multiply (unfortunately, this can come
  // with a major rounding error).
  ticks_ = ticks_.get_d() * scale;
#endif
  return *this;
}

//
// Divide.
//
timestamp& timestamp::operator/=(double scale)
{
#ifndef SSTMAC_USE_GMPXX
  double approximate = this->sec() / scale;
  if(approximate < min_time() || approximate > max_time()) {
    cerrn << "operator/=:  Result " << approximate
              << " is out of range " << min_time() << ".."
              << max_time() << "\n";
    throw sprockit::time_error("timestamp::operator/=():  Result out of range.");
  }
  ticks_ /= scale;
#else
  // Same stupid gmpixx rounding rules as with multiply.
  ticks_ = ticks_.get_d() / scale;
#endif
  return *this;
}

timestamp operator+(const timestamp &a, const timestamp &b)
{
  timestamp rv(a);
  rv += b;
  return rv;
}

timestamp operator-(const timestamp &a, const timestamp &b)
{
  timestamp rv(a);
  rv -= b;
  return rv;
}

timestamp operator*(const timestamp &t, double scaling)
{
  timestamp rv(t);
  rv *= scaling;
  return rv;
}

timestamp operator*(double scaling, const timestamp &t)
{
  return (t * scaling);
}

timestamp operator/(const timestamp &t, double scaling)
{
  timestamp rv(t);
  rv /= scaling;
  return rv;
}

std::ostream& operator<<(std::ostream &os, const timestamp &t)
{
  /*  timestamp::tick_t psec = t.ticks() / t.tick_interval();
    timestamp::tick_t frac = psec % timestamp::tick_t(1e12);
    timestamp::tick_t secs = psec / timestamp::tick_t(1e12);
    os << "timestamp(" << secs << "."
       << std::setw(12) << std::setfill('0') << frac << " sec)";*/

  os << "timestamp(" << t.sec() << " secs)";
  return os;
}

std::string
to_printf_type(timestamp t)
{
  return sprockit::printf("%8.4e msec", t.msec());
}

} // end of namespace sstmac


