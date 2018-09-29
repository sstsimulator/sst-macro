#pragma sst memoize skeletonize(false) model(linear) inputs(niter)
void memoFxn(int niter);

void memoFxn(int n){
  for (int i=0; i < n; ++i);
}

int fxn()
{
  int i=0;
  int mul = 0;
  double* x = new double[10];
  int* idx = new int[5];
#pragma sst memoize skeletonize(true) inputs(5) model(linear)
  for (i=0; i < 5; ++i){
    mul *= i;
    for (int j=0; j < 10; ++j){
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
  memoFxn(10);
  memoFxn(i+12);
  return 0;
}

