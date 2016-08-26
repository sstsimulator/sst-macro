#include <mpi.h>
#include <sstmac/variable.h>
#include <cstdio>
#include <cstdlib>

#define sstmac_app_name ddot

int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int n = 100000000;
  DoublePtr a = new Double[n];
  DoublePtr b = new Double[n];
  double c;
  memset(a, 0, sizeof(double)*n);
  memset(b, 0, sizeof(double)*n);
  int idx = 0;
  for (int i=0; i < n; ++i){
    c += a[idx] * b[idx];
  }
  printf("nops=%llu\n", Double::nops);
  printf("c=%f\n", c);
  MPI_Finalize();
  return 0;
}

