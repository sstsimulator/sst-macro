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
  assertThrows(unit, "overrun exception", sprockit::pvt::ser_buffer_overrun,
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
  std::string name() const { return "A"; }
};

class B : public Base,
 public serializable_type<B>
{
  ImplementSerializable(B)
 public:
  std::string name() const { return "B"; }
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

