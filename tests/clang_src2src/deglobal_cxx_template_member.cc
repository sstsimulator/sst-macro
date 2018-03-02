
template <class T> T global;

struct A {

 template <class T>
 static T member; 
 //template <class T> static CppVarTemplate<A,T,false>;

};
template <class T> T A::member(0);
//template <class T> CppVarTemplate<A,T,false> A::memberWrapper;

template <class T>
struct B {
  template <class U>
  static U member;

  void call();
};
template <class T>
template <class U> U B<T>::member(0);

template <class U> B<U> staticGlobal;

void fxn(){
  A::member<int> = 0;
  B<double>::member<int> = 42;
  staticGlobal<float>.call();
  global<long> = 11;
}

template <class T>
void anotherFxn(){
  A::member<T> = 0;
  staticGlobal<T>.call();
}

template <class T, class U>
void again(){
  B<T>::template member<U> = 0;
}



