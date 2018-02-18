void test(double a){
}

#pragma sst null_variable target x
void test(double* x){
}

int fxn()
{
#pragma sst null_variable delete_all
  double* x = new double[10];
  x[0] = 5;
  test(x[5]);
  double other = x[1];
#pragma sst compute
  for (int i=0; i < 5; ++i){
    x[i] *= i;
  }
  test(x);
  return 0;
}

