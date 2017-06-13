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