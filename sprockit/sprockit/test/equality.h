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

#ifndef EQUALITY_H
#define EQUALITY_H

#include <vector>
#include <cmath>

template <class T>
inline bool arrays_equal(size_t n, const T* test, const T* right);


template <class T>
class TestEquals
{

 public:
  static bool equals(size_t n, const T* test, const T* right) {
    return arrays_equal<T>(n, test, right);
  }

  static bool equals(const T& test, const T& right) {
    return test == right;
  }

};



template <> class TestEquals<float>
{

 public:
  static float cutoff;

  static bool equals(float test, float right) {
    // do a relative difference - normalize for large numbers
    return (fabs(test - right) / fabs(right)) < cutoff;
  }

  static bool equals(size_t n, const float* test, const float* right) {
    return arrays_equal<float>(n, test, right);
  }

};



template <> class TestEquals<double>
{

 public:
  static double cutoff;

  static bool equals(double test, double right) {
    // do a relative difference - normalize for large numbers
    return (fabs(test - right) / fabs(right)) < cutoff;
  }

};

template <class T> class TestEquals< std::vector<T> >
{

 public:
  static bool equals(const std::vector<T>& test, const std::vector<T>& asserted) {
    if (test.size() != asserted.size()) {
      return false;
    }

    typename std::vector<T>::const_iterator it, end = test.end();
    typename std::vector<T>::const_iterator ia = asserted.begin();
    for (it=test.begin(); it != end; ++ia, ++it) {
      if ( !(TestEquals<T>::equals(*it, *ia)) ) {
        return false;
      }
    }
    return true;
  }
};

template <class T>
inline bool arrays_equal(size_t n, const T* test, const T* right)
{
  for (size_t i=0; i < n; ++i) {
    if ( !(TestEquals<T>::equals(test[i], right[i])) ) {
      return false;
    }
  }
  return true;
}




#endif // EQUALITY_H