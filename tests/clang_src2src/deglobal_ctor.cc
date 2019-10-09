
static int x = 10;

struct A {
 int y;
 A() : y(x) {}
};

struct B {
 int z;
 B(int x) : z(x) {}
};

B b(x);
B c(10);

struct C {
  template <class... Args>
  C(Args&&... args){}
};

C d(3,x,b,c,4.5);

