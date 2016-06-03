#include <sprockit/test/test.h>
#include <sumi/thread_safe_ptr_type.h>

using namespace sumi;

static int numAdeleted = 0;
static int numBdeleted = 0;

class A : public thread_safe_ptr_type
{
 public:
  typedef thread_safe_refcount_ptr<A> ptr;
  
  virtual ~A(){
    ++numAdeleted;
  }
};

class B : public A
{
 public:
  typedef thread_safe_refcount_ptr<B> ptr;
  
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

