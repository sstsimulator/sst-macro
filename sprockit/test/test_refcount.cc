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

#include <sprockit/test/test.h>
#include <sprockit/ptr_type.h>

using namespace sprockit;

static int numAdeleted = 0;
static int numBdeleted = 0;
static int numCdeleted = 0;

class A : public ptr_type
{
 public:
  typedef refcount_ptr<A> ptr;
  
  virtual ~A(){
    ++numAdeleted;
  }
};

class B : public A
{
 public:
  typedef refcount_ptr<B> ptr;
  
  virtual ~B(){
    ++numBdeleted;
  }
};

void
run_test(UnitTest& unit)
{
  {
  A* aptr1 = new A;
  B* bptr1 = new B;
  B* bptr2 = new B;
  assertEqual(unit, "A refcount", aptr1->nreferences(), 0);
  assertEqual(unit, "B refcount", bptr1->nreferences(), 0);
  
  A::ptr a = aptr1;
  B::ptr b = bptr1;
  assertEqual(unit, "A refcount", a->nreferences(), 1);
  assertEqual(unit, "B refcount", b->nreferences(), 1);
  
  B::ptr b2 = b;
  assertEqual(unit, "B refcount", b->nreferences(), 2);
  assertEqual(unit, "B refcount", b2->nreferences(), 2);
  b = bptr2;
  assertEqual(unit, "B2 refcount", b->nreferences(), 1);
  assertEqual(unit, "B refcount", b2->nreferences(), 1);

  a = bptr2;
  assertEqual(unit, "B2 refcount", a->nreferences(), 2);
  assertEqual(unit, "A deletion", numAdeleted, 1);

  b = 0;
  assertEqual(unit, "B2 refcount", bptr2->nreferences(), 1);
  a = 0;
  assertEqual(unit, "B deletion", numBdeleted, 1);
  }
  
  //after scoping, should have been deleted
  assertEqual(unit, "B deletion", numBdeleted, 2);
}

int 
main(int arc, char** argv)
{
  UnitTest unit;
  SPROCKIT_RUN_TEST_NO_ARGS(run_test, unit);
  return unit.validate(std::cout);
}