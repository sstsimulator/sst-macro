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

#include <sprockit/output.h>
#include <sprockit/units.h>
#include <sprockit/errors.h>
#include <cstdlib>
#include <errno.h>

namespace sprockit {

/// Multiply two 64 bit integer values and check for overflow.
/// This would be two lines if we had 128-bit integers.
int64_t
multiply64(int64_t a, int64_t b, bool &errorflag)
{
  // Use detection of 32-bit overflow to help us.
  //  let
  //      signed    scaling = 1 << 32
  //      unsigned  mask = scaling >> 1
  //      signed    au = a / scaling
  //      unsigned  al = a & mask
  //      signed    bu = b / scaling
  //      unsigned  bl = b & mask
  //  then
  //      a * b = (au << 32 + al) * (bu << 32 + bl)
  //            = (au * bu << 64) + ((au * bl + bu * al) << 32) + (al * bl)
  // so we have an overflow if any of the following hold:
  //      au and bu are both non-zero
  //      au*bl + bu*al is greater than 0x7fffffff (now that au or bu is zero)
  //      al * bl is greater than 0x7fffffff
  //      the sum ((au*bl + bu*al) << 32) + (al*bl) overflows
  int64_t scaling = int64_t(1) << 32;
  int64_t mask = scaling - 1;
  int64_t max32 = scaling >> 1;
  uint64_t max64 = (~uint64_t(0)) >> 1;
  int64_t au = a / scaling, bu = b / scaling;
  int64_t al = a & mask, bl = b & mask;
  int64_t mul1 = au * bl + bu * al;
  uint64_t mul2 = uint64_t(al) * uint64_t(bl);
  uint64_t uval = uint64_t(mul1) * uint64_t(scaling) + mul2;
  if ((au && bu) || (mul1 > max32) || (mul2 > max64) || (uval > max64)) {
    errorflag = true;
    cerr0 << "64-bit integer overflow in " << a << "*" << b << "\n";
  }
  return int64_t(uval);
}

void
populate_bandwidth_names(std::map<std::string, int64_t> &abbrname,
                         std::map<std::string, int64_t> &fullname)
{
  abbrname["bps"] = abbrname["b/s"] = abbrname["b/sec"] = fullname["bits/s"]
                                      = fullname["bits/sec"] = fullname["bits/second"]
                                          = fullname["bits per second"] = 1;
  // Bytes per second.
  abbrname["Bps"] = abbrname["B/s"] = abbrname["B/sec"] = fullname["bytes/s"]
                                      = fullname["bytes/sec"] = fullname["bytes/second"]
                                          = fullname["bytes per second"] = 8;
  // Kilobits per second.
  abbrname["kbps"] = abbrname["Kbps"] = abbrname["kb/s"] = abbrname["Kb/s"]
                                        = fullname["kbits/s"] = fullname["kbits/sec"]
                                            = fullname["kbits/second"] = fullname["kilobits/s"]
                                                = fullname["kilobits/sec"] = fullname["kilobits/second"]
                                                    = fullname["kilobits per second"] = 1000;
  // Kibibits per second.
  abbrname["kibps"] = abbrname["Kibps"] = abbrname["kib/s"]
                                          = abbrname["Kib/s"] = fullname["kibits/s"] = fullname["kibits/sec"]
                                              = fullname["kibits/second"] = fullname["kibibits/s"]
                                                  = fullname["kibibits/sec"] = fullname["kibibits/second"]
                                                      = fullname["kibibits per second"] = 1024;
  // Kilobytes per second.
  abbrname["kBps"] = abbrname["KBps"] = abbrname["kB/s"] = abbrname["KB/s"]
                                        = fullname["kbytes/s"] = fullname["kbytes/sec"]
                                            = fullname["kbytes/second"] = fullname["kilobytes/s"]
                                                = fullname["kilobytes/sec"] = fullname["kilobytes/second"]
                                                    = fullname["kilobytes per second"] = 1000 * 8;
  // Kibibytes per second.
  abbrname["kiBps"] = abbrname["KiBps"] = abbrname["kiB/s"]
                                          = abbrname["KiB/s"] = fullname["kibytes/s"] = fullname["kibytes/sec"]
                                              = fullname["kibytes/second"] = fullname["kibibytes/s"]
                                                  = fullname["kibibytes/sec"] = fullname["kibibytes/second"]
                                                      = fullname["kibibytes per second"] = 1024 * 8;
  // Megabits per second.
  abbrname["mbps"] = abbrname["Mbps"] = abbrname["mb/s"] = abbrname["Mb/s"]
                                        = fullname["mbits/s"] = fullname["mbits/sec"]
                                            = fullname["mbits/second"] = fullname["megabits/s"]
                                                = fullname["megabits/sec"] = fullname["megabits/second"]
                                                    = fullname["megabits per second"] = 1000 * 1000;
  // Mebibits per second.
  abbrname["mibps"] = abbrname["Mibps"] = abbrname["mib/s"]
                                          = abbrname["Mib/s"] = fullname["mibits/s"] = fullname["mibits/sec"]
                                              = fullname["mibits/second"] = fullname["mebibits/s"]
                                                  = fullname["mebibits/sec"] = fullname["mebibits/second"]
                                                      = fullname["mebibits per second"] = 1024 * 1024;
  // Megabytes per second.
  abbrname["mBps"] = abbrname["MBps"] = abbrname["mB/s"] = abbrname["MB/s"]
                                        = fullname["mbytes/s"] = fullname["mbytes/sec"]
                                            = fullname["mbytes/second"] = fullname["megabytes/s"]
                                                = fullname["megabytes/sec"] = fullname["megabytes/second"]
                                                    = fullname["megabytes per second"] = 1000 * 1000 * 8;
  // Mebibytes per second.
  abbrname["miBps"] = abbrname["MiBps"] = abbrname["miB/s"]
                                          = abbrname["MiB/s"] = fullname["mibytes/s"] = fullname["mibytes/sec"]
                                              = fullname["mibytes/second"] = fullname["mebibytes/s"]
                                                  = fullname["mebibytes/sec"] = fullname["mebibytes/second"]
                                                      = fullname["mebibytes per second"] = 1024 * 1024 * 8;
  // Gigabits per second.
  abbrname["gbps"] = abbrname["Gbps"] = abbrname["gb/s"] = abbrname["Gb/s"]
                                        = fullname["gbits/s"] = fullname["gbits/sec"]
                                            = fullname["gbits/second"] = fullname["gigabits/s"]
                                                = fullname["gigabits/sec"] = fullname["gigabits/second"]
                                                    = fullname["gigabits per second"] = 1000 * 1000 * 1000;
  // Gibibits per second.
  abbrname["gibps"] = abbrname["Gibps"] = abbrname["gib/s"]
                                          = abbrname["Gib/s"] = fullname["gibits/s"] = fullname["gibits/sec"]
                                              = fullname["gibits/second"] = fullname["gibibits/s"]
                                                  = fullname["gibibits/sec"] = fullname["gibibits/second"]
                                                      = fullname["gibibits per second"] = 1024 * 1024 * 1024;
  // Gigabytes per second.
  abbrname["gBps"] = abbrname["GBps"] = abbrname["gB/s"] = abbrname["GB/s"]
                                        = fullname["gbytes/s"] = fullname["gbytes/sec"]
                                            = fullname["gbytes/second"] = fullname["gigabytes/s"]
                                                = fullname["gigabytes/sec"] = fullname["gigabytes/second"]
                                                    = fullname["gigabytes per second"] = 1000LL * 1000LL
                                                        * 1000LL * 8LL;
  // Gibibytes per second.
  abbrname["giBps"] = abbrname["GiBps"] = abbrname["giB/s"]
                                          = abbrname["GiB/s"] = fullname["gibytes/s"] = fullname["gibytes/sec"]
                                              = fullname["gibytes/second"] = fullname["gibibytes/s"]
                                                  = fullname["gibibytes/sec"] = fullname["gibibytes/second"]
                                                      = fullname["gibibytes per second"] = 1024LL * 1024LL
                                                          * 1024LL * 8;
  // Terabits per second.
  abbrname["tbps"] = abbrname["Tbps"] = abbrname["tb/s"] = abbrname["Tb/s"]
                                        = fullname["tbits/s"] = fullname["tbits/sec"]
                                            = fullname["tbits/second"] = fullname["terabits/s"]
                                                = fullname["terabits/sec"] = fullname["terabits/second"]
                                                    = fullname["terabits per second"] = 1000LL * 1000LL
                                                        * 1000LL * 1000LL;
  // Tebibits per second.
  abbrname["gibps"] = abbrname["Gibps"] = abbrname["gib/s"]
                                          = abbrname["Gib/s"] = fullname["tebits/s"] = fullname["tebits/sec"]
                                              = fullname["tebits/second"] = fullname["tebibits/s"]
                                                  = fullname["tebibits/sec"] = fullname["tebibits/second"]
                                                      = fullname["tebibits per second"] = 1024LL * 1024LL
                                                          * 1024LL * 1024LL;
  // Terabytes per second.
  abbrname["tBps"] = abbrname["TBps"] = abbrname["tB/s"] = abbrname["TB/s"]
                                        = fullname["tbytes/s"] = fullname["tbytes/sec"]
                                            = fullname["tbytes/second"] = fullname["terabytes/s"]
                                                = fullname["terabytes/sec"] = fullname["terabytes/second"]
                                                    = fullname["terabytes per second"] = 1000LL * 1000LL
                                                        * 1000LL * 1000LL * 8LL;
  // Tebibytes per second.
  abbrname["tiBps"] = abbrname["TiBps"] = abbrname["tiB/s"]
                                          = abbrname["TiB/s"] = fullname["tibytes/s"] = fullname["tibytes/sec"]
                                              = fullname["tibytes/second"] = fullname["tebibytes/s"]
                                                  = fullname["tebibytes/sec"] = fullname["tebibytes/second"]
                                                      = fullname["tebibytes per second"] = 1024LL * 1024LL
                                                          * 1024LL * 8;
  // Petabits per second.
  abbrname["pbps"] = abbrname["Pbps"] = abbrname["pb/s"] = abbrname["Pb/s"]
                                        = fullname["pbits/s"] = fullname["pbits/sec"]
                                            = fullname["pbits/second"] = fullname["petabits/s"]
                                                = fullname["petabits/sec"] = fullname["petabits/second"]
                                                    = fullname["petabits per second"] = 1000LL * 1000LL
                                                        * 1000LL * 1000LL * 1000LL;
  // Pebibits per second.
  abbrname["gibps"] = abbrname["Gibps"] = abbrname["gib/s"]
                                          = abbrname["Gib/s"] = fullname["pebits/s"] = fullname["pebits/sec"]
                                              = fullname["pebits/second"] = fullname["pebibits/s"]
                                                  = fullname["pebibits/sec"] = fullname["pebibits/second"]
                                                      = fullname["pebibits per second"] = 1024LL * 1024LL
                                                          * 1024LL * 1024LL * 1024LL;
  // Petabytes per second.
  abbrname["tBps"] = abbrname["TBps"] = abbrname["tB/s"] = abbrname["TB/s"]
                                        = fullname["tbytes/s"] = fullname["tbytes/sec"]
                                            = fullname["tbytes/second"] = fullname["petabytes/s"]
                                                = fullname["petabytes/sec"] = fullname["petabytes/second"]
                                                    = fullname["petabytes per second"] = 1000LL * 1000LL
                                                        * 1000LL * 1000LL * 1000LL * 8LL;
  // Pebibytes per second.
  abbrname["piBps"] = abbrname["PiBps"] = abbrname["piB/s"]
                                          = abbrname["PiB/s"] = fullname["pibytes/s"] = fullname["pibytes/sec"]
                                              = fullname["pibytes/second"] = fullname["pebibytes/s"]
                                                  = fullname["pebibytes/sec"] = fullname["pebibytes/second"]
                                                      = fullname["pebibytes per second"] = 1024LL * 1024LL
                                                          * 1024LL * 8;
}


double
get_bandwidth(const char *value, bool &errorflag, bool print_errors)
{
  // This map does case-sensitive matching of abbreviated names
  static std::map<std::string, int64_t> abbrname;
  // and this map does case-insenstive matching of non-abbreviated names.
  static std::map<std::string, int64_t> fullname;
  // Populate the maps with the multipliers for all recognized units.
  // We store the multipliers in bits/second to make everything integers.
  // Bits per second.
  if (abbrname.empty() || fullname.empty()) {
    populate_bandwidth_names(abbrname, fullname);
  }
  // Go.
  char *endptr = NULL;
  double multiplier = 1;
  double val = strtod(value, &endptr);
  errno = 0;
  errorflag = false;
  // Figure out whether we have a value.
  if (*value == '\0' || endptr == value) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid bandwidth value: " << value << "\n";
    return -1;
  }
  if (errno == EINVAL) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid characters in value: " << value << "\n";
    return -1;
  }
  if (errno == ERANGE) {
    errorflag = true;
    if (print_errors) cerr0 << "Value outside 64-bit integer range: " << value << "\n";
    return -1;
  }

  while (*endptr == ' ') {
    ++endptr;
  }

  if (*endptr != '\0') {
    std::string units(endptr);
    std::string lcunits = units;
    for (unsigned i = 0; i < lcunits.size(); ++i) {
      lcunits[i] = tolower(lcunits[i]);
    }
    if (abbrname.find(units) != abbrname.end()) {
      multiplier = abbrname[units];
    }
    else if (fullname.find(lcunits) != fullname.end()) {
      multiplier = fullname[units];
    }
    else {
      errorflag = true;
      if (print_errors) cerr0 << "No match for the bandwidth units " << units << "\n";
      return -1;
    }
  }

  val *= multiplier;
  val /= 8.0; // because the simulator works in bytes/sec.

  return val;
}

void
populate_frequency_names(std::map<std::string, int64_t> &value)
{
  // Accept ps, ns, us, ms, and s with various (lower-case) forms
  // Store time in picoseconds.
  value["hz"] = value["Hz"] = value["HZ"] = 1;
  value["khz"] = value["Khz"] = value["KHz"] = value["HZ"]
                                = 1000;
  value["mhz"] = value["Mhz"] = value["MHz"] = value["MHZ"]
                                = 1000LL * 1000LL;
  value["ghz"] = value["Ghz"] = value["GHz"] = value["GHZ"]
                                = 1000LL * 1000LL * 1000LL;
}

void
populate_length_names(std::map<std::string, int64_t> &value)
{
  // Accept ps, ns, us, ms, and s with various (lower-case) forms
  // Store time in picoseconds.
  value["B"] = value["bytes"] = value["Bytes"] = 1;
  int64_t one_KB = 1000;
  value["KB"] = value["Kbytes"] = value["KBytes"] = one_KB;
  int64_t one_MB = one_KB * 1000LL;
  value["MB"] = value["Mbytes"] = value["MBytes"] = one_MB;
  int64_t one_GB = one_MB * 1000LL;
  value["GB"] = value["Gbytes"] = value["GBytes"] = one_GB;
  int64_t one_TB = one_GB * 1000LL;
  value["TB"] = value["Tbytes"] = value["TBytes"] = one_TB;
  int64_t one_KiB = 1024;
}

void
populate_timestamp_names(std::map<std::string, double> &value)
{
  // Accept ps, ns, us, ms, and s with various (lower-case) forms
  // Store time in picoseconds.
  value["ps"] = value["psec"] = value["pseconds"] = value["picoseconds"] = 1e-12;
  value["ns"] = value["nsec"] = value["nseconds"] = value["nanoseconds"] = 1e-9;
  value["us"] = value["usec"] = value["useconds"] = value["microseconds"] = 1e-6;
  value["ms"] = value["msec"] = value["mseconds"] = value["milliseconds"] = 1e-3;
  value["s"] = value["sec"] = value["seconds"] = 1;
}

long
byte_length(const char* value, bool& errorflag, bool print_errors)
{
  static std::map<std::string, int64_t> mulmap;
  if (mulmap.empty()) {
    populate_length_names(mulmap);
  }

  char *endptr = NULL;
  int64_t multiplier = 1;
  double val = strtod(value, &endptr);
  errno = 0;
  errorflag = false;
  // Figure out whether we have a value.
  if (*value == '\0' || endptr == value) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid time value: " << value << "\n";
    return -1;
  }
  if (errno == EINVAL) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid characters in value: " << value << "\n";
    return -1;
  }
  if (errno == ERANGE) {
    errorflag = true;
    if (print_errors) cerr0 << "Value outside 64-bit integer range: " << value << "\n";
    return -1;
  }

  while (*endptr == ' ') {
    ++endptr;
  }
  if (*endptr != '\0') {
    std::string units(endptr);
    if (mulmap.find(units) != mulmap.end()) {
      multiplier = mulmap[units];
    }
    else {
      errorflag = true;
      if (print_errors) cerr0 << "Invalid time units: " << units << "\n";
      return -1;
    }
  }

  val *= multiplier;

  return val;
}

/// Get a timestamp possiblly suffixed with any of the identifiers
/// psec, nsec, usec, msec, sec, ps, ns, us, ms, s
double
get_timestamp(const char *value, bool &errorflag, bool print_errors)
{
  static std::map<std::string, double> mulmap;
  if (mulmap.empty()) {
    populate_timestamp_names(mulmap);
  }

  char *endptr = NULL;
  double multiplier = 1;
  double val = strtod(value, &endptr);
  errno = 0;
  errorflag = false;

  // Figure out whether we have a value.
  if (*value == '\0' || endptr == value) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid time value: " << value << "\n";
    return -1;
  }
  if (errno == EINVAL) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid characters in value: " << value << "\n";
    return -1;
  }
  if (errno == ERANGE) {
    errorflag = true;
    if (print_errors) cerr0 << "Value outside 64-bit integer range: " << value << "\n";
    return -1;
  }

  while (*endptr == ' ') {
    ++endptr;
  }
  if (*endptr != '\0') {
    std::string units(endptr);
    for (unsigned i = 0; i < units.size(); ++i) {
      units[i] = tolower(units[i]);
    }
    if (mulmap.find(units) != mulmap.end()) {
      multiplier = mulmap[units];
    }
    else {
      errorflag = true;
      if (print_errors) cerr0 << "Invalid time units: " << units << "\n";
      return -1;
    }
  }
  val *= multiplier;

  return val; //return value in seconds
}

/// Get a frequency possibly suffixed with any of the identifiers
/// hz, khz, mhz, ghz, Mhz, Khz, Ghz, Hz, MHz, KHz, GHz
double
get_frequency(const char *value, bool &errorflag, bool print_errors)
{
  static std::map<std::string, int64_t> mulmap;
  if (mulmap.empty()) {
    populate_frequency_names(mulmap);
  }

  char *endptr = NULL;
  int64_t multiplier = 1;
  double val = strtod(value, &endptr);
  errno = 0;
  errorflag = false;

  // Figure out whether we have a value.
  if (*value == '\0' || endptr == value) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid time value: " << value << "\n";
    return -1;
  }
  if (errno == EINVAL) {
    errorflag = true;
    if (print_errors) cerr0 << "Invalid characters in value: " << value << "\n";
    return -1;
  }
  if (errno == ERANGE) {
    errorflag = true;
    if (print_errors) cerr0 << "Value outside 64-bit integer range: " << value << "\n";
    return -1;
  }

  while (*endptr == ' ') {
    ++endptr;
  }
  if (*endptr != '\0') {
    std::string units(endptr);
    for (unsigned i = 0; i < units.size(); ++i) {
      units[i] = tolower(units[i]);
    }
    if (mulmap.find(units) != mulmap.end()) {
      multiplier = mulmap[units];
    }
    else {
      errorflag = true;
      if (print_errors) cerr0 << "Invalid frequency units: " << units << "\n";
      return -1;
    }
  }

  val *= multiplier;

  return val;
}

double
get_bandwidth(const char *value)
{
  bool err = false;
  return get_bandwidth(value, err, false);
}

}