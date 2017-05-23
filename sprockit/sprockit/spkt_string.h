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

#ifndef SPKT_STRING_H
#define SPKT_STRING_H

#include <sstream>
#include <sprockit/spkt_config.h>
#if !SPKT_HAVE_CPP11 && defined(SPKT_HAVE_BOOST)
#include <boost/algorithm/string.hpp>
#else
#include <cctype>
#include <algorithm>
#endif

namespace sprockit {

#define spkt_cstr(x) x->to_string().c_str()

template <class A>
void
spkt_to_stream(
  std::ostream& os,
  const A& a)
{
  os << a;
}

template <class A, class B>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b)
{
  os << a << b;
}

template <class A, class B, class C>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c)
{
  os << a << b << c;
}

template <class A, class B, class C, class D>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d)
{
  os << a << b << c
       << d;
}

template <class A, class B, class C, class D,
          class E>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e)
{
  os << a << b << c
       << d << e;
}

template <class A, class B, class C, class D,
          class E, class F>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f)
{
  os << a << b << c
       << d << e << f;
}

template <class A, class B, class C, class D,
          class E, class F, class G>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g)
{
  os << a << b << c
       << d << e << f
       << g;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h)
{
  os << a << b << c
       << d << e << f
       << g << h;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i)
{
  os << a << b << c
     << d << e << f
     << g << h << i;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I, class J>
void
spkt_to_stream(
  std::ostream& os,
  const char* file, int line,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i,
  const J& j)
{
  os << a << b << c
       << d << e << f
       << g << h << i
       << j;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I, class J, class K>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i,
  const J& j,
  const K& k)
{
  os << a << b << c
       << d << e << f
       << g << h << i
       << j << k;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I, class J, class K, class L>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i,
  const J& j,
  const K& k,
  const L& l)
{
  os << a << b << c
       << d << e << f
       << g << h << i
       << j << k << l;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I, class J, class K, class L,
          class M>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i,
  const J& j,
  const K& k,
  const L& l,
  const M& m)
{
  os << a << b << c
       << d << e << f
       << g << h << i
       << j << k << l
       << m;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I, class J, class K, class L,
          class M, class N>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i,
  const J& j,
  const K& k,
  const L& l,
  const M& m,
  const N& n)
{
  os << a << b << c
       << d << e << f
       << g << h << i
       << j << k << l
       << m << n;
}

template <class A, class B, class C, class D,
          class E, class F, class G, class H,
          class I, class J, class K, class L,
          class M, class N, class O>
void
spkt_to_stream(
  std::ostream& os,
  const A& a,
  const B& b,
  const C& c,
  const D& d,
  const E& e,
  const F& f,
  const G& g,
  const H& h,
  const I& i,
  const J& j,
  const K& k,
  const L& l,
  const M& m,
  const N& n,
  const O& o)
{
  os << a << b << c
       << d << e << f
       << g << h << i
       << j << k << l
       << m << n << o;
}

#define prim_printf_type(ty) \
  inline ty to_pod(ty t){ return t; }

prim_printf_type(const char*)
prim_printf_type(void*)
prim_printf_type(double)
prim_printf_type(float)
prim_printf_type(unsigned long)
prim_printf_type(long)
prim_printf_type(unsigned int)
prim_printf_type(int)
prim_printf_type(unsigned short)
prim_printf_type(short)
prim_printf_type(unsigned long long)
prim_printf_type(long long)

inline const char*
to_pod(const std::string& str){
  return str.c_str();
}

std::string
printf(const char* fmt, ...);

#if SPKT_HAVE_CPP11 || defined(SPKT_HAVE_BOOST)

template<typename WritableRangeT>
void to_upper(WritableRangeT& input)
{
#if SPKT_HAVE_CPP11
  for(char& ch : input) {
    ch = (char)std::toupper(ch);
  }
#elif defined(SPKT_HAVE_BOOST)
  boost::algorithm::to_upper(input);
#elif SPKT_DISABLE_REGEXP
  //do nothing
#else
#  error "Either C++11 or Boost is required to build sprockit"
#endif
}

template<typename WritableRangeT>
void to_lower(WritableRangeT& input)
{
#if SPKT_HAVE_CPP11
  for(char& ch : input) {
    ch = (char)std::tolower(ch);
  }
#elif defined(SPKT_HAVE_BOOST)
  boost::algorithm::to_lower(input);
#else
#  error "Either C++11 or Boost is required to build sprockit"
#endif
}

template<typename SequenceT>
void trim(SequenceT& input)
{
#if SPKT_HAVE_CPP11
  auto check_isspace = [](decltype(*input.begin())& ch) { return (bool)std::isspace(ch); };
  const auto& first_non_space = std::find_if_not(input.begin(), input.end(), check_isspace);
  if(first_non_space == input.end()) {
    input = SequenceT(input.begin(), input.begin()+1);
  }
  else {
    const auto& last_non_space = std::find_if_not(input.rbegin(), input.rend(), check_isspace);
    if(last_non_space == input.rbegin() + 1) {
      input = SequenceT(first_non_space, input.end());
    }
    else {
      input = SequenceT(first_non_space, last_non_space.base() + 1);
    }
  }
#elif defined(SPKT_HAVE_BOOST)
  boost::algorithm::trim(input);
#else
#  error "Either C++11 or Boost is required to build sprockit"
#endif
}

#endif

}


#endif // SPKT_STRING_H