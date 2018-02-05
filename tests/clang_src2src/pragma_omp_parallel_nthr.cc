
int fxn()
{
  int i=0;
  int mul = 0;
  double* x = new double[10];
  int* idx = new int[5];
  for (i=0; i < 5; ++i){
    mul *= i;
#pragma omp parallel for nthread(4)
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
  int anInteger = 45;
  int ret = 0;
#pragma omp parallel for nthread(anInteger)
  for (int i=0; i < 1000; ++i){
    ret += 2 * x[i%10];
  }
  
#pragma omp parallel nthread(6)
  {
  }

  return 0;
}

