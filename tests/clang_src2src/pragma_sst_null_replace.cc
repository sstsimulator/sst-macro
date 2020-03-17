
#pragma sst null_ptr safe
void test(double* x){
}

int fxn()
{
#pragma sst null_ptr replace(0)
  double* x = new double[10];
  test(x);
  return 0;
}

