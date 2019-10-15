
int x[] = {1,2,3};
int xx[42];

namespace y {
  int x[] = {1,2};
  int xx[42];
}

void fxn()
{
  int* z = x;
  int* zz = y::x;
  y::xx[0] = 0;
  xx[0] = 0;
}

