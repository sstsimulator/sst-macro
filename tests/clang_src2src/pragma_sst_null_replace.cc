
void test(double* x){
}

int fxn()
{
#pragma sst null_variable replace 0
  double* x = new double[10];
  test(x);
  return 0;
}

