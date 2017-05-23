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

double TestEquals<double>::cutoff = 1e-12;
float TestEquals<float>::cutoff = 1e-12;

int
UnitTest::validate(std::ostream &os)
{
  int nfailed = 0;
  for (int i=0; i < tests_.size(); ++i) {
    TestCase* test = tests_[i];
    bool success = test->is_correct();
    if (success) {
      os << "SUCCESS: ";
      test->print_descr(os);
    }
    else {
      ++nfailed;
      os << "FAILURE: ";
      test->print_descr(os);
      os << "  ";
      test->print_error(os);
      os << "\n\t     Test: ";
      test->print_test_value(os);
      os << "\n\t Asserted: ";
      test->print_asserted_value(os);
    }
    os << "\n";
  }
  return nfailed;
}

void _assertFalse(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  bool test)
{
  AssertEqual<bool,bool>::add_test(test_set, descr, file, line, test, false,
                                   true);
}

void _assertTrue(UnitTest& test_set,
                 const char* descr,
                 const char* file,
                 int line,
                 bool test)
{
  AssertEqual<bool,bool>::add_test(test_set, descr, file, line, test, true, true);
}

const char*
truncate_file(const char* file)
{
  int len = ::strlen(file);
  const char* charptr = file + len;
  while (charptr != file) {
    --charptr;
    if (*charptr == '/') {
      return (charptr+1);
    }
  }
  return file;
}