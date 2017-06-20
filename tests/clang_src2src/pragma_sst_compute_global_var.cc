
int bound_i = 5;
int bound_j = 10;

int fxn()
{
  int i=0;
  int mul = 0;
  double* x = new double[10];
  int* idx = new int[5];
#pragma sst compute
  for (i=0; i < bound_i; ++i){
    mul *= i;
    for (int j=0; j < bound_j; ++j){
      mul += (j-1);
      x[j] += i;
      mul -= x[j];
      j=7;
      mul += x[j];
      mul *= x[idx[i]];
      idx[i] -= 3;
      mul *= x[idx[i]];
    }
  }
  return 0;
}

