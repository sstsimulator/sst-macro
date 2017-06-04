
struct A {
 static int x;
 void fxn(){
  x -= 1;
 }
};

void fxn()
{
  A::x = 10;
};

