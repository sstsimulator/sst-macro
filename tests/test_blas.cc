#include <sstmac/util.h>
#include <sstmac/libraries/blas/cblas.h>
#include <sstmac/software/process/app.h>
#include <sstmac/skeleton.h>

#define run_fxn(fxn, ...) \
{  \
  double t_start, t_stop, t_total; \
  t_start = sstmac_now(); \
  fxn(__VA_ARGS__); \
  t_stop = sstmac_now(); \
  t_total = t_stop - t_start; \
  std::string fxn_name = sprockit::printf("T(%s)", #fxn); \
  std::cout << sprockit::printf("%10s = %12.8f ms\n", fxn_name.c_str(), 1e3*t_total); \
}


extern "C" int ubuntu_cant_name_mangle() { return 0; }


#define sstmac_app_name test_blas

int USER_MAIN(int argc, char** argv)
{
  run_fxn(sstmac_simple_daxpy,10000);
  run_fxn(sstmac_simple_dgemv,1000,1000);
  run_fxn(sstmac_simple_dgemm,1000,1000,1000);
  run_fxn(sstmac_simple_ddot,10000);
  return 0;
}
