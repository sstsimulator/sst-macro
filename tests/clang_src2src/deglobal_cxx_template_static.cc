/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

template <class T>
struct A {
 T& getX(){
  return x;
 }
 static T x;
 void fxn(){
  x -= 1;
 }
 T& getXagain(){
  return x;
 }
};
template <class T> T A<T>::x = 5;

namespace ns {
template <class T, class U>
struct B {
 static int x;
};
template <class T, class U> int B<T,U>::x(42);
}

template <class T, const char* tag>
class C {
 static int value;
 static T anotherValue;
};

template <class T, const char* tag> int C<T,tag>::value;
template <class T, const char* tag> T C<T,tag>::anotherValue;

class E {};

template <typename>
class D {
  static int var;
  static E e;
};
template <class T> int D<T>::var;
template <class T> E D<T>::e = {};


template <class T>
struct F {
  static A<int> a;
};
template <class T> A<int> F<T>::a;

template <class T>
struct G {
  using MemberType = A<int>;
  static MemberType a;
  static int b;
};
template <class T> typename G<T>::MemberType G<T>::a;
template <class T> int G<T>::b = 0;

void fxn(){
  G<int>::a.fxn();
  G<double>::b += 1;
}



