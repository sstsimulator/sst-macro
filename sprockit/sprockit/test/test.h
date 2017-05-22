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

#ifndef TEST_H
#define TEST_H

#include <vector>
#include <iostream>
#include <string.h>
#include <sprockit/test/output.h>
#include <sprockit/test/equality.h>
#include <sprockit/test/container.h>
#include <sprockit/test/fxn.h>
#include <sprockit/spkt_string.h>

const char*
truncate_file(const char* file);

class TestCase
{
 public:
  virtual bool is_correct() const = 0;

  virtual void print_error(std::ostream& os) const = 0;

  virtual void print_descr(std::ostream& os) const = 0;

  virtual void print_test_value(std::ostream& os) const = 0;

  virtual void print_asserted_value(std::ostream& os) const = 0;

};

template <class T>
class TestCase_impl :
  public TestCase
{
 private:
  std::string descr_;
  bool should_be_equal_;
  bool equal_;
  T test_;
  T asserted_;

 public:
  TestCase_impl(const std::string& descr,
                bool should_be_equal,
                const T& test,
                const T& asserted)
    : descr_(descr),
      should_be_equal_(should_be_equal),
      equal_(false),
      test_(test),
      asserted_(asserted) {
    equal_ = TestEquals<T>::equals(test_, asserted_);
  }

  bool is_correct() const {
    return equal_ == should_be_equal_;
  }

  void print_error(std::ostream &os) const {
    if (should_be_equal_) {
      os << "SHOULD BE EQUAL";
    }
    else {
      os << "SHOULD NOT BE EQUAL";
    }
  }

  void
  print_descr(std::ostream& os) const {
    os << descr_;
  }

  void
  print_test_value(std::ostream& os) const {
    ClassOutput<T>::print(test_, os);
  }

  void
  print_asserted_value(std::ostream& os) const {
    ClassOutput<T>::print(asserted_, os);
  }

};

class UnitTest
{

 private:
  std::vector<TestCase*> tests_;

 public:
  void append(TestCase* test_case) {
    tests_.push_back(test_case);
  }

  /**
      @return The number of failed tests
  */
  int validate(std::ostream& os = std::cout);

  static int
  exit_status(int nfailed) {
    return nfailed == 0 ? 0 : 1;
  }

};

template <class T, class A>
class AssertEqual
{
 public:
  static void
  add_test(UnitTest& test_set,
           const char* descr,
           const char* file,
           int line,
           const T& test,
           const A& asserted,
           bool should_be_equal = true) {
    A casted(test);
    std::string info = sprockit::printf("%s %s:%d", descr, file, line);
    test_set.append(new TestCase_impl<A>(info,should_be_equal,casted,asserted));
  }
};

template <class T>
class AssertEqual<T,T>
{
 public:
  static void
  add_test(UnitTest &test_set,
           const char *descr,
           const char* file,
           int line,
           const T& test,
           const T& asserted,
           bool should_be_equal = true) {
    std::string info = sprockit::printf("%s %s:%d", descr, file, line);
    test_set.append(new TestCase_impl<T>(info,should_be_equal,test,asserted));
  }
};


template <class T, class A>
class AssertEqual< std::vector<T>, A>
{
 public:
  static void
  add_test(UnitTest& test_set,
           const char* descr,
           const char* file,
           int line,
           const std::vector<T>& test,
           const A& asserted,
           bool should_be_equal = true) {
    std::vector<T> asserted_vec;
    asserted_vec.push_back(asserted);
    std::string info = sprockit::printf("%s %s:%d", descr, file, line);
    test_set.append(new TestCase_impl<std::vector<T> >(info,should_be_equal,test,
                    asserted_vec));
  }
};

template <class T>
class AssertEqual< std::vector<T>, std::vector<T> >
{
 public:
  static void
  add_test(UnitTest& test_set,
           const char* descr,
           const char* file,
           int line,
           const std::vector<T>& test,
           const std::vector<T>& asserted,
           bool should_be_equal = true) {
    std::string info = sprockit::printf("%s %s:%d", descr, file, line);
    test_set.append(new TestCase_impl<std::vector<T> >(info,should_be_equal,test,
                    asserted));
  }
};

void _assertFalse(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  bool test);

void _assertTrue(UnitTest& test_set,
                 const char* descr,
                 const char* file,
                 int line,
                 bool test);

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& asserted,
                  bool should_be_equal)
{
  AssertEqual<T,A>::add_test(test_set, descr, file, line, test, asserted,
                             should_be_equal);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  bool should_be_equal)
{
  T assert_container;
  ContainerAppend<T,A>::append(assert_container, assert1);
  ContainerAppend<T,A>::append(assert_container, assert2);
  AssertEqual<T,T>::add_test(test_set, descr, file, line,  test, assert_container,
                             should_be_equal);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  bool should_be_equal)
{
  T assert_container;
  ContainerAppend<T,A>::append(assert_container, assert1);
  ContainerAppend<T,A>::append(assert_container, assert2);
  ContainerAppend<T,A>::append(assert_container, assert3);
  AssertEqual<T,T>::add_test(test_set, descr, file, line,  test, assert_container,
                             should_be_equal);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  const A& assert4,
                  bool should_be_equal)
{
  T assert_container;
  ContainerAppend<T,A>::append(assert_container, assert1);
  ContainerAppend<T,A>::append(assert_container, assert2);
  ContainerAppend<T,A>::append(assert_container, assert3);
  ContainerAppend<T,A>::append(assert_container, assert4);
  AssertEqual<T,T>::add_test(test_set, descr, file, line,  test, assert_container,
                             should_be_equal);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  const A& assert4,
                  const A& assert5,
                  bool should_be_equal)
{
  T assert_container;
  ContainerAppend<T,A>::append(assert_container, assert1);
  ContainerAppend<T,A>::append(assert_container, assert2);
  ContainerAppend<T,A>::append(assert_container, assert3);
  ContainerAppend<T,A>::append(assert_container, assert4);
  ContainerAppend<T,A>::append(assert_container, assert5);
  AssertEqual<T,T>::add_test(test_set, descr, file, line,  test, assert_container,
                             should_be_equal);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  const A& assert4,
                  const A& assert5,
                  const A& assert6,
                  bool should_be_equal)
{
  T assert_container;
  ContainerAppend<T,A>::append(assert_container, assert1);
  ContainerAppend<T,A>::append(assert_container, assert2);
  ContainerAppend<T,A>::append(assert_container, assert3);
  ContainerAppend<T,A>::append(assert_container, assert4);
  ContainerAppend<T,A>::append(assert_container, assert5);
  ContainerAppend<T,A>::append(assert_container, assert6);
  AssertEqual<T,T>::add_test(test_set, descr, file, line,  test, assert_container,
                             should_be_equal);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& asserted)
{
  _assertEqual(test_set, descr, file, line,  test, asserted, true);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, true);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               true);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  const A& assert4)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               assert4, true);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  const A& assert4,
                  const A& assert5)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               assert4, assert5, true);
}

template <class T, class A>
void _assertEqual(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  const T& test,
                  const A& assert1,
                  const A& assert2,
                  const A& assert3,
                  const A& assert4,
                  const A& assert5,
                  const A& assert6)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               assert4, assert5, assert6, true);
}

template <class T, class A>
void _assertNotEqual(UnitTest& test_set,
                     const char* descr,
                     const char* file,
                     int line,
                     const T& test,
                     const A& asserted)
{
  _assertEqual(test_set, descr, file, line,  test, asserted, false);
}

template <class T, class A>
void _assertNotEqual(UnitTest& test_set,
                     const char* descr,
                     const char* file,
                     int line,
                     const T& test,
                     const A& assert1,
                     const A& assert2)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, false);
}

template <class T, class A>
void _assertNotEqual(UnitTest& test_set,
                     const char* descr,
                     const char* file,
                     int line,
                     const T& test,
                     const A& assert1,
                     const A& assert2,
                     const A& assert3)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               false);
}

template <class T, class A>
void _assertNotEqual(UnitTest& test_set,
                     const char* descr,
                     const char* file,
                     int line,
                     const T& test,
                     const A& assert1,
                     const A& assert2,
                     const A& assert3,
                     const A& assert4)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               assert4, false);
}

template <class T, class A>
void _assertNotEqual(UnitTest& test_set,
                     const char* descr,
                     const char* file,
                     int line,
                     const T& test,
                     const A& assert1,
                     const A& assert2,
                     const A& assert3,
                     const A& assert4,
                     const A& assert5)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               assert4, assert5, false);
}

template <class T, class A>
void _assertNotEqual(UnitTest& test_set,
                     const char* descr,
                     const char* file,
                     int line,
                     const T& test,
                     const A& assert1,
                     const A& assert2,
                     const A& assert3,
                     const A& assert4,
                     const A& assert5,
                     const A& assert6)
{
  _assertEqual(test_set, descr, file, line,  test, assert1, assert2, assert3,
               assert4, assert5, assert6, false);
}

class ExceptionTestCase :
  public TestCase
{
 private:
  std::string descr_;
  const char* file_;
  int line_;
  std::string exc_;
  std::string test_;
  std::string errormsg_;
  bool success_;

 public:
  ExceptionTestCase(const std::string& descr,
                    const char* file,
                    int line,
                    const std::string& exc,
                    const std::string& test,
                    const std::string& errormsg,
                    bool success)
    : descr_(descr),
      file_(file),
      line_(line),
      exc_(exc),
      errormsg_(errormsg),
      success_(success) {
    int last_split = 0;
    const char* data = test.c_str();
    for (int i=0; i < test.size(); ++i){
      if (data[i] == '\n'){
        int length = i - last_split;
        test_ += test.substr(last_split, length) + "\n                    ";
        last_split = i+1; //plus one to skip new line
      }
    }
    test_ += test.substr(last_split);
  }

  bool is_correct() const {
    return success_;
  }

  void print_error(std::ostream& os) const {
    os << errormsg_;
  }

  void print_descr(std::ostream& os) const {
    os << descr_ << " "
       << file_ << ":"
       << line_;
  }

  void print_test_value(std::ostream& os) const {
    os << test_;
  }

  void print_asserted_value(std::ostream& os) const {
    os << exc_;
  }

};

template <class Exc>
void assertThrows_impl(UnitTest& test_set, const char* descr, const char* file,
                       int line, const char* exc, TestFxn* fxn)
{
  try {
    fxn->run();
    test_set.append(new ExceptionTestCase(descr, file, line, exc, "No Exception",
                                          "SHOULD THROW", false));
  }
  catch(Exc) {
    test_set.append(new ExceptionTestCase(descr, file, line, exc, exc,
                                          "SHOULD THROW", true));
  }
  catch (std::exception& e) {
    test_set.append(new ExceptionTestCase(descr, file, line, exc, e.what(),
                                          "WRONG EXCEPTION THROWN", false));
  }
  catch(...) {
    test_set.append(new ExceptionTestCase(descr, file, line, exc,
                                          "Unknown Exception", "WRONG EXCEPTION THROWN", false));
  }
}

#define THIS_FILE truncate_file(__FILE__)

#define SPROCKIT_RUN_TEST(fxn, unit_tester, ...) \
  try { \
    fxn(unit_tester, __VA_ARGS__); \
  } catch (std::exception& e) { \
    unit_tester.append(new ExceptionTestCase("unexpected exception in " #fxn, THIS_FILE, __LINE__, "none", e.what(), "exception thown", false)); \
  }

#define SPROCKIT_RUN_TEST_NO_ARGS(fxn, unit_tester) \
  try { \
    fxn(unit_tester); \
  } catch (std::exception& e) { \
    unit_tester.append(new ExceptionTestCase("unexpected exception in " #fxn, THIS_FILE, __LINE__, "none", e.what(), "exception thown", false)); \
  }

#define assertThrows(test_set, descr, exc, fxn) \
    assertThrows_impl<exc>(test_set, descr, THIS_FILE, __LINE__, #exc, fxn)

#define assertEqual(test_set, descr, ...) \
    _assertEqual(test_set, descr, THIS_FILE, __LINE__, __VA_ARGS__)

#define assertNotEqual(test_set, descr, ...) \
    _assertNotEqual(test_set, descr, THIS_FILE, __LINE__, __VA_ARGS__)

#define assertTrue(test_set, descr, ...) \
    _assertTrue(test_set, descr, THIS_FILE, __LINE__, __VA_ARGS__)

#define assertFalse(test_set, descr, ...) \
    _assertFalse(test_set, descr, THIS_FILE, __LINE__, __VA_ARGS__)

#endif // TEST_H