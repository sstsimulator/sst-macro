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
#include <sprockit/test/test.h>
#include <stdexcept>


class error1 : public std::runtime_error
{
    public:
    error1(const std::string& err)
        : std::runtime_error(err)
    {
    }
};

class error2 : public std::runtime_error
{
    public:
    error2(const std::string& err)
        : std::runtime_error(err)
    {
    }
};

template <class T>
void throw_exc(int arg){
    throw T("error");
}

void dont_throw_exc(int arg)
{
}

void throw_int(int arg)
{
    throw arg;
}

class MemberTestClass
{
    public:
        template <class T>
        void
        throw_exc(int arg)
        {
            throw T("error");
        }

        void
        dont_throw_exc(int arg)
        {
        }

        void
        throw_int(int arg)
        {
            throw arg;
        }

};

int main(int argc, char** argv)
{
    UnitTest unit;
    assertEqual(unit, "int equality", 3, 3);
    assertEqual(unit, "int equality", 3, 4);
    assertEqual(unit, "float equality", 3.14, 3.14);

    {
    std::vector<int> vec; fillContainer(vec, 1, 2, 3, 4);
    assertEqual(unit, "fill vector", vec, 1, 2, 3, 4);
    }

    {
    std::vector<int> vec; fillContainer(vec, 1, 2, 3, 4);
    assertNotEqual(unit, "fill vector", vec, 1, 2, 4, 4);
    }

    assertFalse(unit, "test false", false);
    assertTrue(unit, "test true", true);

    assertThrows(unit, "does throw", error1,
                 static_fxn(&throw_exc<error1>, 0));
    assertThrows(unit, "doesn't throw", error1,
                 static_fxn(&dont_throw_exc, 0));
    assertThrows(unit, "throws wrong", error2,
                 static_fxn(&throw_exc<error1>, 0));
    assertThrows(unit, "throws int", error2,
                 static_fxn(&throw_int, 20));

    {
    MemberTestClass* cls = new MemberTestClass;
    assertThrows(unit, "ptr member does throw", error1,
                 member_fxn(cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "ptr member doesn't throw", error1,
                 member_fxn(cls, &MemberTestClass::dont_throw_exc, 0));
    assertThrows(unit, "ptr member throws wrong", error2,
                 member_fxn(cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "ptr member throws int", error2,
                 member_fxn(cls, &MemberTestClass::throw_int, 20));
    }

    {
    MemberTestClass cls;
    assertThrows(unit, "member does throw", error1,
                 member_fxn(&cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "member doesn't throw", error1,
                 member_fxn(&cls, &MemberTestClass::dont_throw_exc, 0));
    assertThrows(unit, "member throws wrong", error2,
                 member_fxn(&cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "member throws int", error2,
                 member_fxn(&cls, &MemberTestClass::throw_int, 20));
    }

    unit.validate();
    //return UnitTest::exit_status(unit.validate());
    return 0;
}
