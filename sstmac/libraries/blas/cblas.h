#ifndef sstmac_libraries_blas_CBLAS_H
#define sstmac_libraries_blas_CBLAS_H

#ifdef __cplusplus
extern  "C" {
#endif

void
sstmac_simple_dgemm(int m, int n, int k);

void
sstmac_dgemm(char *transa, char *transb, int* m, int* n, int* k,
  double* alpha, double* a, int* lda,
  double* b, int* ldb, double* beta,
  double* c, int* ldc);

void
sstmac_simple_dgemv(int m, int n);

void
sstmac_dgemv(char *transa, int* m, int* n,
  double* alpha, double* a, int* lda,
  double* x, int* incx, double* beta,
  double* y, int* incy);

void
sstmac_simple_daxpy(int n);

void
sstmac_daxpy(int* n,
  double* alpha,
  double* x, int* incx,
  double* y, int* incy);

void
sstmac_simple_ddot(int n);

void
sstmac_ddot(int* n,
  double* x, int* incx,
  double* y, int* incy);

#ifdef __cplusplus
}
#endif

#endif // CBLAS_H
