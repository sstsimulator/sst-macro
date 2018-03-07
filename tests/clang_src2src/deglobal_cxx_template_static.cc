
template <class T>
struct A {
 T& getX(){
  return x;
 }
 static T x;
 void fxn(){
  x -= 1;
 }
 T& getXagain(){
  return x;
 }
};
template <class T> T A<T>::x = 5;

namespace ns {
template <class T, class U>
struct B {
 static int x;
};
template <class T, class U> int B<T,U>::x(42);
}

template <class T, const char* tag>
class C {
 static int value;
 static T anotherValue;
};

template <class T, const char* tag> int C<T,tag>::value;
template <class T, const char* tag> T C<T,tag>::anotherValue;

class E {};

template <typename>
class D {
  static int var;
  static E e;
};
template <class T> int D<T>::var;
template <class T> E D<T>::e = {};


template <class T>
struct F {
  static A<int> a;
};
template <class T> A<int> F<T>::a;

template <class T>
struct G {
  using MemberType = A<int>;
  static MemberType a;
  static int b;
};
template <class T> typename G<T>::MemberType G<T>::a;
template <class T> int G<T>::b = 0;

void fxn(){
  G<int>::a.fxn();
  G<double>::b += 1;
}



