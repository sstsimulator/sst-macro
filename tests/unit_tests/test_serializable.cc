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

