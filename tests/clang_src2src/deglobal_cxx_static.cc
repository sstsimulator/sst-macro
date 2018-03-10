
struct A {
 static int x;
 void fxn(){
  x -= 1;
 }
};

int initFxn();

struct Caller {
  Caller(int a){}
};

namespace ns {
struct B {
 static int x;
 static Caller caller;
};
namespace ns2 {
 struct C {
  static int xx;
 };
}
}

int ns::B::x = 42;
Caller ns::B::caller = initFxn();
int A::x = 5;
namespace ns {
 namespace ns2 {
  int C::xx = 10;
 }
}

decltype(ns::B::caller) dtGlbl(0);

void fxn()
{
  ns::B::x += 1;
  ns::ns2::C::xx -= 3;
  A::x = 10;
};

struct DeclTyper {
  static int get(){ return 0; }
};
DeclTyper d;

int goReturn(){
  return decltype(d)::get();
}


struct D {
  static int arr[4];
};
int D::arr[4] = {1,2,3,4};


