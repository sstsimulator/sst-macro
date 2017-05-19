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

#include <sstmac/util.h>
#include <sprockit/serializer.h>
#include <sprockit/output.h>
#include <sprockit/test/test.h>
#include <sprockit/debug.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/messages/sst_message.h>

sstmac::node_id
naddr(long nid)
{
  return sstmac::node_id(nid);
}

using namespace sstmac;
using namespace sstmac::sw;

char buf[4096];

template<class T>
  T
  cycle(const T& msg, UnitTest &unit)
  {
    typedef typename T::element_type PTR;
    message* new_msg = ser_cycle(msg, unit);
    return ptr_safe_cast(PTR, new_msg);
  }

template <class A, class B, class C, class D, class E>
void
ser_prim_test(UnitTest& unit, sprockit::serializer& ser, A a, B b, C c, D d, E e)
{
  //first get the size
  ser.set_mode(sprockit::serializer::SIZER);
  ser & a;
  ser & b;
  ser & c;
  ser & d;
  ser & e;
  size_t correct_size = ser.sizer().size();

  ser.packer().reset();
  ser.set_mode(sprockit::serializer::PACK);
  ser & a;
  ser & b;
  ser & c;
  ser & d;
  ser & e;
  size_t test_size = ser.packer().size();
  assertEqual(unit, "serialization size", test_size, correct_size);

  A atest; B btest; C ctest; D dtest; E etest;
  ser.set_mode(sprockit::serializer::UNPACK);
  ser.unpacker().reset();
  ser & atest;
  ser & btest;
  ser & ctest;
  ser & dtest;
  ser & etest;
  test_size = ser.unpacker().size();
  assertEqual(unit, "serialization size", test_size, correct_size);

  assertEqual(unit, "element a", atest, a);
  assertEqual(unit, "element b", btest, b);
  assertEqual(unit, "element c", ctest, c);
  assertEqual(unit, "element d", dtest, d);
  assertEqual(unit, "element e", etest, e);
}

int
main(int argc, char** argv)
{

  UnitTest unit;

  try
  {
    sprockit::serializer ser;
    int bufsize = 512;
    char* buf = new char[bufsize];
    ser.packer().init(buf, bufsize);
    ser.unpacker().init(buf, bufsize);
    ser_prim_test(unit, ser, 12L, 13, double(100.532), std::string("booyah"), uint32_t(42));

  }
  catch (std::exception& e)
  {
    cerr0 << e.what() << std::endl;
    return 1;
  }

  unit.validate();

  return 0;

}
