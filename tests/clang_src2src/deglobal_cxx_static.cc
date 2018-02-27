
struct A {
 static int x;
 void fxn(){
  x -= 1;
 }
};

namespace ns {
struct B {
 static int x;
};
namespace ns2 {
 struct C {
  static int x;
 };
}
}

int ns::B::x = 42;
int A::x = 5;
namespace ns {
 namespace ns2 {
  int C::x = 10;
 }
}

void fxn()
{
  ns::B::x += 1;
  ns::ns2::C::x -= 3;
  A::x = 10;
};

struct DeclTyper {
  static int get(){ return 0; }
};
DeclTyper d;

int goReturn(){
  return decltype(d)::get();
}


