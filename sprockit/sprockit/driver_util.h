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

#ifndef SPROCKIT_DRIVER_UTIL_H
#define SPROCKIT_DRIVER_UTIL_H

#include <vector>

namespace sprockit {

double
get_positive_float(const char *value, bool &errorflag);

int
get_int(const char *value, bool &errorflag);

int
get_positive_int(const char *value, bool &errorflag);

void
get_intvec(const char *value, bool &errorflag, std::vector<int>& retval);

std::vector<int>
get_intvec(const char *value, bool &errorflag);

}
#endif

