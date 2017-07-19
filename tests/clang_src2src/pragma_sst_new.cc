
void fxn1()
{
#pragma sst new
  double* x = new double[10];
#pragma sst new
  int* idx = new int[5];
}

void fxn2()
{
  #pragma sst new
  double* x = new double[10];
  int* idx = new int[5];
}

