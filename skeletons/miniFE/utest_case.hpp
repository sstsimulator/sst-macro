/*
//@HEADER
// ************************************************************************
// 
//               HPCCG: Simple Conjugate Gradient Benchmark Code
//                 Copyright (2006) Sandia Corporation
// 
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
// 
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//  
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//  
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER
*/

#ifndef _utest_case_hpp_
#define _utest_case_hpp_

#include <vector>

class utest_case;

std::vector<utest_case*>& get_utest_cases()
{
  static std::vector<utest_case*> utest_cases;
  return utest_cases;
}

//When a class that inherits the utest_case class is constructed,
//it gets added to the vector of utest_cases returned by
//the above 'get_utest_cases' function.
class utest_case {
public:
  utest_case(){ get_utest_cases().push_back(this); }
  ~utest_case(){}
  virtual const char* name() = 0;
  virtual bool run() = 0;
};

//The following macro declares and instantiates a class that
//inherits the above utest_case interfaces.
//
//use the macro like this:
//   UTEST_CASE(mytest)
//   {
//      ... test code here ...
//   }
//
//See example usages in utest_cases.hpp
//
#define UTEST_CASE(TESTNAME) \
  class TESTNAME##_utest : public utest_case { \
  public: \
    TESTNAME##_utest(){} \
    const char* name() {return #TESTNAME;} \
    bool run(); \
  }; \
  \
  TESTNAME##_utest instance_##TESTNAME##_utest; \
  \
  bool TESTNAME##_utest::run()

#define TEST_EQUAL(A,B) \
  if ((A) != (B)) return false;

#define TEST_EQUAL_TOL(A,B,tol) \
  if (std::abs((A) - (B)) > tol) return false;

#endif

