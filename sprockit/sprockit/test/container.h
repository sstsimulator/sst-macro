#ifndef CONTAINER_H
#define CONTAINER_H

#include <map>
#include <vector>
#include <sprockit/test/assert.h>

template <class K, class V>
class kv
{
 public:
  kv(const K& k, const V& v)
    : key(k), value(v) {
  }

  K key;
  V value;
};


template <class C, class A>
class ContainerAppend
{
 public:
  static void
  append(C& c, const A& a) {
    invalidContainer(c);
  }
};

template <class T, class A>
class ContainerAppend<std::vector<T>, A>
{
 public:
  static void
  append(std::vector<T>& t, const A& a) {
    t.push_back(a);
  }
};

template <class K, class V>
class ContainerAppend<std::map<K,V>, kv<K,V> >
{
 public:
  static void
  append(std::map<K,V>& m, const kv<K,V>& pair) {
    m[pair.key] = pair.value;
  }
};


template <class C, class A>
void fillContainer(C& t, A& a1)
{
  ContainerAppend<C,A>::append(t, a1);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2, const A& a3)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2, const A& a3, const A& a4)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
  ContainerAppend<C,A>::append(t, a4);
}

template <class C, class A>
void fillContainer(C& t, const A& a1, const A& a2, const A& a3, const A& a4,
                   const A& a5)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
  ContainerAppend<C,A>::append(t, a4);
  ContainerAppend<C,A>::append(t, a5);
}

template <class C, class A>
void fillContainer(C& t, A& a1, A& a2, A& a3, A& a4, A& a5, A& a6)
{
  ContainerAppend<C,A>::append(t, a1);
  ContainerAppend<C,A>::append(t, a2);
  ContainerAppend<C,A>::append(t, a3);
  ContainerAppend<C,A>::append(t, a4);
  ContainerAppend<C,A>::append(t, a5);
  ContainerAppend<C,A>::append(t, a6);
}


#endif // CONTAINER_H

