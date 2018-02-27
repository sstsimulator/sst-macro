
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





