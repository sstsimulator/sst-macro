#include <sprockit/test/test.h>
#include <sprockit/errors.h>
#include <sumi/thread_safe_int.h>
#include <sumi/thread_safe_list.h>
#include <sumi/thread_safe_set.h>

using sumi::thread_safe_int;
using sumi::thread_safe_list;
using sumi::thread_safe_set;

// very simple functionality tests that do not fully exercise the
// thread safety features

void
test_thread_safe_int(UnitTest& unit)
{
  int val = 3;
  int val2 = 8;

  {
    thread_safe_int<int> a(val);
    int aa = a;
    assertEqual(unit, "constructor / operator Integer()", aa, val);

    a = val2;
    aa = a;
    assertEqual(unit, "operator=(Integer)", aa, val2);

    thread_safe_int<int> b(val);
    a = b;
    aa = a;
    assertEqual(unit, "operator=(TSI)", aa, val);

    thread_safe_int<int> c(b);
    aa = c;
    assertEqual(unit, "copy constructor", aa, val);
  }

  {
    thread_safe_int<int> a(val);
    a += val2;
    int aa = a;
    assertEqual(unit, "operator+=()", aa, val+val2);
  }

  {
    thread_safe_int<int> a(val);
    a -= val2;
    int aa = a;
    assertEqual(unit, "operator-=()", aa, val-val2);
  }

  {
    thread_safe_int<int> a(val);
    a |= val2;
    int aa = a;
    assertEqual(unit, "operator|=()", aa, val|val2);
  }

  {
    thread_safe_int<int> a(val);
    int aa = ++a;
    int aaa = a;
    assertEqual(unit, "operator++()", aa, val+1);
    assertEqual(unit, "operator++()", aaa, val+1);
  }

  {
    thread_safe_int<int> a(val);
    int aa = a++;
    int aaa = a;
    assertEqual(unit, "operator++(int)", aa, val);
    assertEqual(unit, "operator++(int)", aaa, val+1);
  }

  {
    thread_safe_int<int> a(val);
    int aa = --a;
    int aaa = a;
    assertEqual(unit, "operator--()", aa, val-1);
    assertEqual(unit, "operator--()", aaa, val-1);
  }

  {
    thread_safe_int<int> a(val);
    int aa = a--;
    int aaa = a;
    assertEqual(unit, "operator--(int)", aa, val);
    assertEqual(unit, "operator--(int)", aaa, val-1);
  }

  {
    thread_safe_int<int> a(val);
    thread_safe_int<int> b(val2);
    thread_safe_int<int> c(val);

    assertFalse(unit, "operator==(TSI)", a == b);
    assertTrue(unit, "operator!=(TSI)", a != b);
    assertTrue(unit, "operator<(TSI)", a < b);
    assertTrue(unit, "operator<=(TSI)", a <= b);
    assertFalse(unit, "operator>(TSI)", a > b);
    assertFalse(unit, "operator>=(TSI)", a >= b);

    assertFalse(unit, "operator<(TSI)", b < a);
    assertFalse(unit, "operator<=(TSI)", b <= a);
    assertTrue(unit, "operator>(TSI)", b > a);
    assertTrue(unit, "operator>=(TSI)", b >= a);

    assertTrue(unit, "operator==(TSI)", a == c);
    assertFalse(unit, "operator!=(TSI)", a != c);
    assertFalse(unit, "operator<(TSI)", a < c);
    assertTrue(unit, "operator<=(TSI)", a <= c);
    assertFalse(unit, "operator>(TSI)", a > c);
    assertTrue(unit, "operator>=(TSI)", a >= c);

    assertTrue(unit, "operator==(TSI)", a == a);
    assertFalse(unit, "operator!=(TSI)", a != a);
    assertFalse(unit, "operator<(TSI)", a < a);
    assertTrue(unit, "operator<=(TSI)", a <= a);
    assertFalse(unit, "operator>(TSI)", a > a);
    assertTrue(unit, "operator>=(TSI)", a >= a);

    assertFalse(unit, "operator==(TSI)", a == val2);
    assertTrue(unit, "operator!=(TSI)", a != val2);
    assertTrue(unit, "operator<(TSI)", a < val2);
    assertTrue(unit, "operator<=(TSI)", a <= val2);
    assertFalse(unit, "operator>(TSI)", a > val2);
    assertFalse(unit, "operator>=(TSI)", a >= val2);

    assertTrue(unit, "operator==(TSI)", a == val);
    assertFalse(unit, "operator!=(TSI)", a != val);
    assertFalse(unit, "operator<(TSI)", a < val);
    assertTrue(unit, "operator<=(TSI)", a <= val);
    assertFalse(unit, "operator>(TSI)", a > val);
    assertTrue(unit, "operator>=(TSI)", a >= val);
  }
}

void
test_thread_safe_list(UnitTest& unit)
{
  int val = 4;

  typedef thread_safe_list<int> list_t;
  typedef list_t::iterator iterator_t;
  typedef list_t::const_iterator const_iterator_t;

  {
    list_t list;
    assertTrue(unit, "empty() when empty", list.empty());

    list.push_back(val);
    assertFalse(unit, "empty() when not empty", list.empty());
  }

  {
    list_t list;
    bool empty;
    int popped = list.pop_front_and_return(empty);
    assertTrue(unit, "pop_front_and_return() shows empty", empty);

    list.push_back(val);
    popped = list.pop_front_and_return(empty);
    assertFalse(unit, "pop_front_and_return() shows non-empty", empty);
    assertEqual(unit, "pop_front_and_return() returned correct value", popped, val);
  }

  {
    list_t list;
    bool empty;
    int popped = list.pop_back_and_return(empty);
    assertTrue(unit, "pop_back_and_return() shows empty", empty);

    list.push_back(val);
    popped = list.pop_back_and_return(empty);
    assertFalse(unit, "pop_back_and_return() shows non-empty", empty);
    assertEqual(unit, "pop_back_and_return() returned correct value", popped, val);
  }

  {
    list_t list;

    list.lock();
    assertTrue(unit, "empty() when empty", list.empty_locked());
    list.unlock();

    list.push_back(15);
    list.push_back(val);
    list.push_back(1);

    list.lock();
    assertFalse(unit, "empty() when not empty", list.empty_locked());
    list.unlock();

    int count = 0;
    iterator_t end = list.start_iteration();
    for (iterator_t it = list.begin(); it != end; ++it) {
      count++;
    }
    assertEqual(unit, "iterates over correct count", count, 3);

    for (iterator_t it = list.begin(); it != end; ++it) {
      if (*it == val) {
        list.insert(it, 25);
        break;
      }
    }
    count = 0;
    for (iterator_t it = list.begin(); it != end; ++it) {
      count++;
    }
    assertEqual(unit, "inserted", count, 4);

    for (iterator_t it = list.begin(); it != end; ++it) {
      if (*it == val) {
        list.erase(it);
        break;
      }
    }
    count = 0;
    for (iterator_t it = list.begin(); it != end; ++it) {
      count++;
    }
    assertEqual(unit, "erased", count, 3);
    assertEqual(unit, "size", list.size_locked(), 3);
    list.end_iteration();
    assertEqual(unit, "size", list.size(), 3);

    const list_t &list2 = list;
    const_iterator_t cend = list2.start_iteration();
    count = 0;
    for (const_iterator_t it = list2.begin(); it != cend; ++it) {
      count++;
    }
    assertEqual(unit, "const iterator", count, 3);
    list2.end_iteration();

    list_t list3(list);
    iterator_t end3 = list3.start_iteration();
    count = 0;
    for (iterator_t it = list3.begin(); it != end3; ++it) {
      count++;
    }
    assertEqual(unit, "copy constructor", count, 3);
    list3.end_iteration();

    list_t list4;
    list4 = list;
    iterator_t end4 = list4.start_iteration();
    count = 0;
    for (iterator_t it = list4.begin(); it != end4; ++it) {
      count++;
    }
    assertEqual(unit, "operator=", count, 3);
    list4.end_iteration();

    list.clear();

    list.start_iteration();
    count = 0;
    for (iterator_t it = list.begin(); it != end; ++it) {
      count++;
    }
    assertEqual(unit, "cleared iterates over nothing", count, 0);

    list.end_iteration();

    list.push_back(val);
    list.start_iteration();
    list.clear_locked();
    count = 0;
    for (iterator_t it = list.begin(); it != end; ++it) {
      count++;
    }
    assertEqual(unit, "cleared iterates over nothing", count, 0);
    assertEqual(unit, "cleared size 0", list.size_locked(), 0);
    list.end_iteration();
    assertEqual(unit, "cleared size 0", list.size(), 0);
    assertTrue(unit, "cleared empty", list.empty());
  }
}

void
test_thread_safe_set(UnitTest& unit)
{
  int val = 4, val2 = 15;

  typedef thread_safe_set<int> set_t;
  typedef set_t::iterator iterator_t;
  typedef set_t::const_iterator const_iterator_t;

  {
    set_t s;
    assertTrue(unit, "empty() when empty", s.empty());

    s.insert(val);
    assertFalse(unit, "empty() when not empty", s.empty());
  }

  {
    set_t s;

    s.lock();
    assertTrue(unit, "empty() when empty", s.empty_locked());
    s.unlock();

    s.insert(val2);
    s.insert(val);
    s.insert(1);

    s.lock();
    assertFalse(unit, "empty() when not empty", s.empty_locked());
    s.unlock();

    assertEqual(unit, "counted 1 instance", s.count(val), 1);
    assertEqual(unit, "counted 0 instances", s.count(99), 0);

    std::string str = s.to_string();
    assertTrue(unit, "to_string", str.compare("{ 1 4 15 }") == 0);

    int count = 0;
    iterator_t end = s.start_iteration();
    for (iterator_t it = s.begin(); it != end; ++it) {
      count++;
    }
    s.end_iteration();
    assertEqual(unit, "iterates over correct count", count, 3);

    int newval = 25;
    s.insert(newval);

    end = s.start_iteration();
    count = 0;
    bool succ = false;
    for (iterator_t it = s.begin(); it != end; ++it) {
      count++;
      if (*it == newval)
        succ = true;
    }
    assertEqual(unit, "iterated over more after inserting", count, 4);
    assertTrue(unit, "iterated over newly-inserted value", succ);
    
    for (iterator_t it = s.begin(); it != end; ++it) {
      if (*it == val) {
        s.erase(it);
        break;
      }
    }

    count = 0;
    succ = true;
    for (iterator_t it = s.begin(); it != end; ++it) {
      count++;
      if (*it == newval)
        succ = false;
    }
    assertEqual(unit, "iterated over fewer after erasing", count, 3);
    assertFalse(unit, "newly-inserted value was erased", succ);
    s.end_iteration();

    assertEqual(unit, "erased by value", s.erase(val2), 1);

    const set_t &s2 = s;
    const_iterator_t cend = s2.start_iteration();
    count = 0;
    for (const_iterator_t it = s2.begin(); it != cend; ++it) {
      count++;
    }
    assertEqual(unit, "const iterated over fewer after erasing", count, 2);
    s2.end_iteration();

    assertEqual(unit, "didn't erase missing value", s.erase(99), 0);

    cend = s2.start_iteration();
    count = 0;
    for (const_iterator_t it = s2.begin(); it != cend; ++it) {
      count++;
    }
    assertEqual(unit, "const iterated over same after not erasing", count, 2);
    s2.end_iteration();
  }

  {
    set_t s1;
    set_t s2;
    set_t sr;

    s1.insert(5);
    s1.insert(10);
    s1.insert(15);
    s1.insert(20);
    assertEqual(unit, "first set size", s1.size(), 4);
    assertTrue(unit, "first set string", s1.to_string().compare("{ 5 10 15 20 }") == 0);

    s2.insert(20);
    s2.insert(10);
    assertEqual(unit, "second set size", s2.size(), 2);
    assertTrue(unit, "second set string", s2.to_string().compare("{ 10 20 }") == 0);

    set_difference(s1, s2, sr);
    assertEqual(unit, "difference size", sr.size(), 2);
    assertTrue(unit, "difference string", sr.to_string().compare("{ 5 15 }") == 0);

    s2.insert_all(sr);
    assertEqual(unit, "merged set size", s2.size(), 4);
    assertTrue(unit, "merged set string", s2.to_string().compare("{ 5 10 15 20 }") == 0);

    s2.clear();
    assertEqual(unit, "cleared set size", s2.size(), 0);

    {
      iterator_t end = s1.start_iteration();
      iterator_t it = s1.find(10);
      assertTrue(unit, "found entry", it != end);
      assertEqual(unit, "dereferenced iterator", *it, 10);
      s1.end_iteration();
    }

    {
      iterator_t end = s1.start_iteration();
      iterator_t it = s1.find(9);
      assertTrue(unit, "didn't find entry", it == end);
      s1.end_iteration();
    }

    set_t s3(s1);

    {
      iterator_t end = s3.start_iteration();
      iterator_t it = s3.find(10);
      assertTrue(unit, "found entry in copy", it != end);
      assertEqual(unit, "dereferenced iterator", *it, 10);
      s3.end_iteration();
    }

    {
      iterator_t end = s3.start_iteration();
      iterator_t it = s3.find(9);
      assertTrue(unit, "didn't find entry in copy", it == end);
      s3.end_iteration();
    }

    {
      // make sure we don't deadlock using self as argument
      int cnt = s3.size();
      s3.insert_all(s3);
      assertEqual(unit, "inserting self", s3.size(), cnt);
    }

    set_t s4;
    s4 = s1;

    {
      iterator_t end = s4.start_iteration();
      iterator_t it = s4.find(10);
      assertTrue(unit, "found entry in assignment copy", it != end);
      assertEqual(unit, "dereferenced iterator", *it, 10);
      s4.end_iteration();
    }

    {
      iterator_t end = s4.start_iteration();
      iterator_t it = s4.find(9);
      assertTrue(unit, "didn't find entry in assignment copy", it == end);
      s4.end_iteration();
    }

    std::set<int> s5 = s1.get_copy();

    {
      iterator_t it = s5.find(10);
      assertTrue(unit, "found entry in unsafe copy", it != s5.end());
      assertEqual(unit, "dereferenced iterator", *it, 10);
    }

    {
      iterator_t it = s5.find(9);
      assertTrue(unit, "didn't find entry in unsafe copy", it == s5.end());
    }

    set_t s6(s5);

    {
      iterator_t end = s6.start_iteration();
      iterator_t it = s6.find(10);
      assertTrue(unit, "found entry in copy from unsafe", it != end);
      assertEqual(unit, "dereferenced iterator", *it, 10);
      s6.end_iteration();
    }

    {
      iterator_t end = s6.start_iteration();
      iterator_t it = s6.find(9);
      assertTrue(unit, "didn't find entry in copy from unsafe", it == end);
      s6.end_iteration();
    }

    set_t s7;
    s7 = s5;

    {
      iterator_t end = s7.start_iteration();
      iterator_t it = s7.find(10);
      assertTrue(unit, "found entry in assignment copy from unsafe", it != end);
      assertEqual(unit, "dereferenced iterator", *it, 10);
      s7.end_iteration();
    }

    {
      iterator_t end = s7.start_iteration();
      iterator_t it = s7.find(9);
      assertTrue(unit, "didn't find entry in assignment copy from unsafe", it == end);
      s7.end_iteration();
    }

    {
      std::set<int> s7p;
      s7p.insert(12);
      s7.insert_all(s7p);
      iterator_t end = s7.start_iteration();
      iterator_t it = s7.find(12);
      assertTrue(unit, "found entry after insert_all from unsafe", it != end);
      assertEqual(unit, "dereferenced iterator", *it, 12);
      s7.end_iteration();

      std::set<int> sr2;
      set_difference(s7, s6, sr2);
      assertEqual(unit, "difference size", sr2.size(), 1);

      s7.lock();
      s7.clear_locked();
      s7.unlock();
      assertEqual(unit, "cleared set size", s7.size(), 0);
    }
  }
}

int main(int argc, char** argv)
{
  UnitTest unit;
  try {
    SPROCKIT_RUN_TEST_NO_ARGS(test_thread_safe_int, unit);
    SPROCKIT_RUN_TEST_NO_ARGS(test_thread_safe_list, unit);
    SPROCKIT_RUN_TEST_NO_ARGS(test_thread_safe_set, unit);
  } catch (std::exception& e) {
    std::cerr << "Thread-safe test failed to initialize: "
      << e.what() << std::endl;
    return 1;
  }

  return unit.validate();
}
