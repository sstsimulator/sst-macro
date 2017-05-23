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
#include <sprockit/serialize.h>
#include <sprockit/serializable.h>

using namespace sprockit;

void
test_serialize_basic(UnitTest& unit)
{
  static const int size = 512;
  char buf[size];

  sprockit::serializer ser;
  ser.start_packing(buf, size);

  int correct_int = 42;
  double correct_double = 5.15;

  ser & correct_int;
  ser & correct_double;

  int test_int; double test_double;

  ser.start_unpacking(buf, size);
  ser & test_int;
  ser & test_double;

  assertEqual(unit, "test int", test_int, correct_int);
  assertEqual(unit, "test double", test_double, correct_double);
}


void
test_serialize_array(UnitTest& unit)
{
  static const int bufsize = 512;

  int correct_size = 20;
  std::vector<int> correct(correct_size);
  for (int i=0; i < correct_size; ++i){
    correct[i] = i;
  }

  sprockit::serializer ser;

  char buf[bufsize];
  ser.start_packing(buf, bufsize);
  int* correct_array = &correct[0];
  ser & array(correct_array, correct_size);
  void* correct_buffer = correct_array;
  int correct_buffer_size = correct_size*sizeof(int)/sizeof(char);
  ser & array(correct_buffer, correct_buffer_size);



  int test_size = 0;
  int* test_array;
  ser.start_unpacking(buf, bufsize);
  ser & array(test_array, test_size);
  std::vector<int> test(test_array, test_array + test_size);

  assertEqual(unit, "array size", test_size, correct_size);
  assertEqual(unit, "test arrays", test, correct);

  void* test_buffer;
  ser & array(test_buffer, test_size);
  test_size /= sizeof(int)/sizeof(char);
  test_array = (int*) test_buffer;
  std::vector<int> buftest(test_array, test_array + test_size);

  assertEqual(unit, "buffer size", test_size, correct_size);
  assertEqual(unit, "test buffers", buftest, correct);
}

void overpack_buffer()
{
  static const int bufsize = 512;
  char buf[bufsize];
  serializer ser;
  ser.start_packing(buf, bufsize);

  int fits[100];
  ser & fits;

  int doesntfit[100];
  ser & doesntfit;
}

void test_serialize_overrun(UnitTest& unit)
{
  assertThrows(unit, "overrun exception", sprockit::ser_buffer_overrun,
    static_fxn(overpack_buffer));
}

void insert(std::set<int>& s, int* start, int* stop){ s.insert(start, stop); }
void insert(std::vector<int>& s, int* start, int* stop){ s.insert(s.begin(), start, stop); }
void insert(std::list<int>& s, int* start, int* stop){ s.insert(s.begin(), start, stop); }


template <class Map>
void test_serialize_map(UnitTest& unit)
{
  const char* strings[] = { "hello", "world", "again" };
  int vals[] = { 13, 18, 42 };
  int nelems = sizeof(vals) / sizeof(int);

  Map input;
  for (int i=0; i < nelems; ++i){
    input[strings[i]] = vals[i];
  }

  serializer ser;
  ser.start_sizing();
  ser & input;

  int string_size = sizeof(int) + 5;
  int correct_size = sizeof(size_t) + 3*string_size + 3*sizeof(int);
  assertEqual(unit, "sizer size", ser.size(), correct_size);

  char* buffer = new char[ser.size()];
  ser.start_packing(buffer, ser.size());
  ser & input;

  assertEqual(unit, "packed size", ser.size(), correct_size);

  ser.start_unpacking(buffer, ser.size());

  Map output;
  ser & output;

  typename Map::iterator itin, itout, end = input.end();
  for (itin=input.begin(), itout=output.begin(); itin != end; ++itin, ++itout){
    assertEqual(unit, "map key", itin->first, itout->first);
    assertEqual(unit, "map value", itin->second, itout->second);
  }
}

template <class Container>
void test_serialize_container(UnitTest& unit)
{
  int elems[] = {5, 6, 8, 444};
  int nelems = sizeof(elems) / sizeof(int);
  int* start = elems;
  int* stop = start + nelems;
  Container input;
  insert(input, start, stop);

  serializer ser;
  ser.start_sizing();
  ser & input;

  int size = ser.size();
  int correct_size = sizeof(size_t) + sizeof(int)*4;
  assertEqual(unit, "size", size, correct_size);

  char* buffer = new char[size];
  ser.start_packing(buffer, size);
  ser & input;

  assertEqual(unit, "packed size", ser.size(), correct_size);

  ser.start_unpacking(buffer, size);

  Container output;
  ser & output;

  assertEqual(unit, "output list size", output.size(), input.size());
  if (input.size() != output.size())
    return;

  typename Container::iterator itin, itout, end = input.end();
  for (itin=input.begin(), itout=output.begin(); itin != end; ++itin, ++itout){
    assertEqual(unit, "list value", *itout, *itin);
  }
}

class Base : public serializable
{
 public:
  virtual std::string name() const  = 0;

  void serialize_order(serializer &ser){
    ser & x_;
  }

  int x() const { return x_; }

 protected:
  Base() : x_(5) {}

 private:
  int x_;
};

class A : public Base,
 public serializable_type<A>
{
  ImplementSerializable(A)
 public:
  std::string name() const override { return "A"; }
};

class B : public Base,
 public serializable_type<B>
{
  ImplementSerializable(B)
 public:
  std::string name() const override { return "B"; }
};

void
test_serializable(UnitTest& unit)
{
  Base* input = new A;

  serializer ser;
  char buffer[512];
  ser.start_packing(buffer, sizeof(buffer));
  ser & input;

  ser.start_unpacking(buffer, sizeof(buffer));
  Base* output;
  ser & output;

  assertEqual(unit, "serialized class name", output->name(), std::string("A"));
  assertEqual(unit, "serialized class member", output->x(), input->x());

  input = new B;
  ser.start_packing(buffer, sizeof(buffer));
  ser & input;

  ser.start_unpacking(buffer, sizeof(buffer));
  ser & output;

  assertEqual(unit, "serialized class name", output->name(), std::string("B"));
  assertEqual(unit, "serialized class member", output->x(), input->x());
}

int 
main(int arc, char** argv)
{
  UnitTest unit;
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_basic, unit);
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_array, unit);
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_overrun, unit);
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_container<std::list<int> >, unit);
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_container<std::set<int> >, unit);
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_container<std::vector<int> >, unit);
  typedef std::map<std::string, int> STDMap;
  SPROCKIT_RUN_TEST_NO_ARGS(test_serialize_map<STDMap>, unit);
  SPROCKIT_RUN_TEST_NO_ARGS(test_serializable, unit);
  return unit.validate(std::cout);
}