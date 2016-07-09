#include <sstmac/libraries/blas/blas_api.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/launcher.h>


using namespace sstmac;
using namespace sstmac::sw;

static blas_api*
get_lib_blas()
{
  thread* t = operating_system::current_thread();
  return t->get_api<blas_api>();
}

extern "C" void
sstmac_simple_dgemm(int m, int n, int k)
{
  get_lib_blas()->dgemm(m, n, k);
}

extern "C" void
sstmac_dgemm(char *transa, char *transb, int* m, int* n, int* k, 
  double* alpha, double* a, int* lda, 
  double* b, int* ldb, double* beta, 
  double* c, int* ldc
)
{
  sstmac_simple_dgemm(*m, *n, *k);
}

extern "C" void
sstmac_simple_dgemv(int m, int n)
{
  get_lib_blas()->dgemv(m, n);
}

extern "C" void
sstmac_dgemv(char *transa, int* m, int* n,
  double* alpha, double* a, int* lda,
  double* x, int* incx, double* beta,
  double* y, int* incy
)
{
  sstmac_simple_dgemv(*m, *n);
}


extern "C" void
sstmac_simple_daxpy(int n)
{
  get_lib_blas()->daxpy(n);
}

extern "C" void
sstmac_daxpy(int* n,
  double* alpha,
  double* x, int* incx,
  double* y, int* incy
)
{
  sstmac_simple_daxpy(*n);
}

extern "C" void
sstmac_simple_ddot(int n)
{
  get_lib_blas()->ddot(n);
}

extern "C" void
sstmac_ddot(int* n,
  double* x, int* incx,
  double* y, int* incy)
{
  sstmac_simple_ddot(*n);
}

