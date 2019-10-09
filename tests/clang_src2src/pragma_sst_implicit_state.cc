
enum ImplicitStates {
  null=0,
  length,
  size,
  temp
};
 

#pragma sst memoize skeletonize(false) model(linear) inputs(niter)
void memoFxn(int niter);

void memoFxn(int n){
  for (int i=0; i < n; ++i);
}

#pragma sst implicit_state size(param) temp(param)
void testFxn(int param){
}

#pragma sst implicit_state size(param) temp(param)
void anotherTestFxn(int param){
  param += 5;
}

int fxn()
{
  int i=0;
  int mul = 0;
  double* x = new double[10];
  int* idx = new int[5];
#pragma sst memoize skeletonize(true) inputs(5) model(linear) name(forloop)
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
#pragma sst implicit_state temp(mul)
  memoFxn(10);
#pragma sst implicit_state length(5)
  memoFxn(i+12);
  return 0;
}


