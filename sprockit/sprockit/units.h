/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2010 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SPROCKIT_COMMON_UNITS_H_INCLUDED
#define SPROCKIT_COMMON_UNITS_H_INCLUDED

#include <map>
#include <stdint.h>

namespace sprockit {

/// Multiply two 64 bit integer values and check for overflow.
/// This would be two lines if we had 128-bit integers.
int64_t
multiply64(int64_t a, int64_t b, bool &errorflag);

void
populate_bandwidth_names(std::map<std::string, int64_t> &abbrname,
                         std::map<std::string, int64_t> &fullname);

double
get_bandwidth(const char *value, bool &errorflag, bool print_errors = false);

double
get_bandwidth(const char *value);

double
get_frequency(const char* value, bool& errorflag, bool print_errors = false);

long
byte_length(const char* value, bool& errorflag, bool print_errors = false);

void
populate_timestamp_names(std::map<std::string, int64_t> &value);

void
populate_frequency_names(std::map<std::string, int64_t> &value);

/// Get a timestamp possiblly suffixed with any of the identifiers
/// psec, nsec, usec, msec, sec, ps, ns, us, ms, s
double
get_timestamp(const char *value, bool &errorflag, bool print_errors = false);

}

#endif

