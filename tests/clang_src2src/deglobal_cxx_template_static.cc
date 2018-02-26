
template <class T>
struct A {
 static T x;
 void fxn(){
  x -= 1;
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




