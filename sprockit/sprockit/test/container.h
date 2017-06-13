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

#ifndef CONTAINER_H
#define CONTAINER_H

#include <map>
#include <vector>
#include <sprockit/test/assert.h>

template <class K, class V>
class kv
{
 public:
  kv(const K& k, const V& v)
    : key(k), value(v) {
  }

  K key;
  V value;
};


template <class C, class A>
class ContainerAppend
{
 public:
  static void
  append(C& c, const A& a) {
    invalidContainer(c);
  }
};

template <class T, class A>
class ContainerAppend<std::vector<T>, A>
{
 public:
  static void
  append(std::vector<T>& t, const A& a) {
    t.push_back(a);
  }
};

template <class K, class V>
class ContainerAppend<std::map<K,V>, kv<K,V> >
{
 public:
  static void
  append(std::map<K,V>& m, const kv<K,V>& pair) {
    m[pair.key] = pair.value;
  }
};


template <class C, class A>
void fillContainer(C& t, A& a1)
{
  ContainerAppend<C,A>::append(t, a1);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2, const A& a3)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2, const A& a3, const A& a4)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
  ContainerAppend<C,A>::append(t, a4);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2, const A& a3, const A& a4,
                   const A& a5)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
  ContainerAppend<C,A>::append(t, a4);
  ContainerAppend<C,A>::append(t, a5);
}

template <class C, class A>
void fillContainer(C& t, A& a1, A& a2, A& a3, A& a4, A& a5, A& a6)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
  ContainerAppend<C,A>::append(t, a4);
  ContainerAppend<C,A>::append(t, a5);
  ContainerAppend<C,A>::append(t, a6);
}


#endif // CONTAINER_H