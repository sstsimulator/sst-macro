
#pragma sst new
void fxn1()
{
  double* x = new double[10];
  int* idx = new int[5];
}

void fxn2()
{
  #pragma sst new
  double* x = new double[10];
  int* idx = new int[5];
}

