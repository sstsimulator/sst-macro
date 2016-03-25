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

#include <sprockit/util.h>
#include <sprockit/output.h>
#include <errno.h>
#include <vector>
#include <limits>
#include <string.h>

namespace sprockit {

double
get_positive_float(const char *value, bool &errorflag)
{
  char *endptr = NULL;
  double val = strtod(value, &endptr);
  // This is really pretty bad form
  errno = 0;
  if (*value == '\0' || *endptr != '\0') {
    errorflag = true;
    cerr0 << "Not a valid floating point value: " << value << "\n";
  }
  if (errno == ERANGE) {
    errorflag = true;
    cerr0 << "Value outside double precision range: " << value << "\n";
  }
  if (val <= 0) {
    errorflag = true;
    cerr0 << "Not a positive floating point value: " << value << "\n";
  }
  return val;
}

int
get_int(const char *value, bool &errorflag)
{
  char *endptr;
  errno = 0;
  long val = strtol(value, &endptr, 10);
  if (*value == '\0' || *endptr != '\0') {
    errorflag = true;
    cerr0 << "Invalid integer value: " << value << "\n";
  }
  if (errno == ERANGE || val > std::numeric_limits<int>::max() || val
      < std::numeric_limits<int>::min()) {
    errorflag = true;
    cerr0 << "Value outside 32-bit integer range: " << value << "\n";
  }
  if (errno == EINVAL) {
    errorflag = true;
    cerr0 << "Value contains invalid characters: " << value << "\n";
  }
  return val;
}

int
get_positive_int(const char *value, bool &errorflag)
{
  int v = get_int(value, errorflag);
  if ((!errorflag) && v <= 0) {
    errorflag = true;
    cerr0 << "Not a positive non-zero integer value: " << value << "\n";
  }
  return v;
}

void
get_intvec(const char *value, bool &errorflag, std::vector<int>& retval)
{
  char *brkt;
  char *val = strdup(value);
  const char *sep = " ,;_-";
  for (char *word = strtok_r(val, sep, &brkt); word != NULL; word = strtok_r(
         NULL, sep, &brkt)) {
    retval.push_back(get_int(word, errorflag));
    if (errorflag) {
      break;
    }
  }
  free(val);
}

std::vector<int>
get_intvec(const char *value, bool &errorflag)
{
  std::vector<int> retval;
  get_intvec(value, errorflag, retval);
  return retval;
}






}

