struct B {
  void fxn();
};

template <class T>
struct A{
  void fxn();
  static B x;
};

template <class T> B A<T>::x;

template <class T>
void
A<T>::fxn(){
#pragma sst global x
  this->x.fxn();
  B var;
}

decltype(A<int>::x) var;


