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

//
// Static variables.
//
timestamp::tick_t timestamp::PSEC_PER_TICK = 1;
timestamp::tick_t timestamp::nanoseconds = 1000;
timestamp::tick_t timestamp::microseconds = 1000 * nanoseconds;
timestamp::tick_t timestamp::milliseconds = 1000 * microseconds;
timestamp::tick_t timestamp::seconds = 1000 * milliseconds; //default is 1 tick per ps
timestamp::tick_t timestamp::minutes = seconds * 60;


static double _double_to_tick_prefactor_ = 1e12;
static double _tick_to_double_prefactor_ = 1e-12;
static std::string _tick_spacing_string_("1 ps");

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

//
// Get the tick interval in std::string form (for example, "1ps").
//
const std::string& timestamp::tick_interval_string()
{
  return _tick_spacing_string_;
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
  return ((std::numeric_limits<int64_t>::max()) * 1e-12) * PSEC_PER_TICK;
}

//
// static:  Get the smallest (most negative) time value possible (in seconds).
//
double timestamp::min_time()
{
  return (std::numeric_limits<int64_t>::min() * 1e-12) * PSEC_PER_TICK;
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
  timestamp retval(0);
  retval.ticks_ = value * scaling / PSEC_PER_TICK;
  return retval;
}

//
// Add.
//
timestamp& timestamp::operator+=(const timestamp &other)
{
  ticks_ += other.ticks_;
  return *this;
}

//
// Subtract.
//
timestamp& timestamp::operator-=(const timestamp &other)
{
  ticks_ -= other.ticks_;
  return *this;
}

//
// Multiply.
//
timestamp& timestamp::operator*=(double scale)
{
  ticks_ *= scale;
  return *this;
}

//
// Divide.
//
timestamp& timestamp::operator/=(double scale)
{
  ticks_ /= scale;
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


