extern int y[][4];

int x[][2] = { {1,2}, {2,3} };
int y[3][4];
extern int z[][2];
static int b[2][2];

void fxn()
{
  //static int a[3][5];
  //a[0][0] = 1;
  x[0][1] = 2;
  y[1][1] = 3;
  z[2][0] = 5;
  b[1][1] = 8;
}


