struct loop {
 int begin;
 int end;
};

extern loop bounds[];

int fxn()
{
  int mul = 0;
  double* x = new double[10];
  int* idx = new int[5];
  int loopMax = 5;
  int sstLoopCount = loopMax/2;
#pragma sst compute
  for (int i=0; i < loopMax; ++i){
    mul *= i;
    #pragma sst loop_count sstLoopCount
    for (int j=bounds[i].begin; j < bounds[i].end; ++j){
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

