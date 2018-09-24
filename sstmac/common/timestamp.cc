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

//
// Static variables.
//
timestamp::tick_t timestamp::PSEC_PER_TICK = 1;
timestamp::tick_t timestamp::nanoseconds = 1000;
timestamp::tick_t timestamp::microseconds = 1000 * nanoseconds;
timestamp::tick_t timestamp::milliseconds = 1000 * microseconds;
timestamp::tick_t timestamp::seconds = 1000 * milliseconds; //default is 1 tick per ps
timestamp::tick_t timestamp::minutes = seconds * 60;
double timestamp::ticks_per_second_;
double timestamp::seconds_per_tick_;
double timestamp::msec_per_tick_;
double timestamp::usec_per_tick_;
double timestamp::nsec_per_tick_;
double timestamp::psec_per_tick_;
double timestamp::max_time_;
double timestamp::min_time_;

static std::string _tick_spacing_string_("1 ps");


void timestamp::init_stamps(tick_t tick_spacing)
{
  static bool inited_ = false;
  if (inited_) return;

  //psec_tick_spacing_ = new tick_t(tick_spacing);
  PSEC_PER_TICK = tick_spacing;
  seconds_per_tick_ = 1e-12 * tick_spacing;
  msec_per_tick_ = 1e-9 * tick_spacing;
  usec_per_tick_ = 1e-6 * tick_spacing;
  nsec_per_tick_ = 1e-3 * tick_spacing;
  psec_per_tick_ = tick_spacing;
  ticks_per_second_ = 1e12 / double(tick_spacing);
  std::stringstream ss;
  ss << tick_spacing << " ps";
  _tick_spacing_string_ = ss.str();
  nanoseconds = 1000 / PSEC_PER_TICK;
  microseconds = (1000 * 1000) / PSEC_PER_TICK;
  milliseconds = (1000 * 1000 * 1000) / PSEC_PER_TICK;
  seconds = (tick_t(1000) * 1000 * 1000 * 1000) / PSEC_PER_TICK;
  minutes = 60 * seconds;

  max_time_ = std::numeric_limits<tick_t>::max() / seconds;
  min_time_ = std::numeric_limits<tick_t>::min() / seconds;
}

//
// Return the current time in seconds.
//
double timestamp::sec() const
{
  return ticks_ * seconds_per_tick_;
}

//
// Return the current time in milliseconds.
//
double timestamp::msec() const
{
  return ticks_ * msec_per_tick_;
}

//
// Return the current time in microseconds.
//
double timestamp::usec() const
{
  return ticks_ * usec_per_tick_;
}

//
// Return the current time in nanoseconds.
//
double timestamp::nsec() const
{
  return ticks_ * nsec_per_tick_;
}

//
// Return the current time in picoseconds.
//
double timestamp::psec() const
{
  return ticks_ * psec_per_tick_;
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
