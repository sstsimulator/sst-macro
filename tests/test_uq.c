#include <sstmac/libraries/uq/uq.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define debug_print(...) 
//#define debug_print(...) printf(__VA_ARGS__)

int main(int argc, char** argv)
{
  int nproc = 64;
  int npartners = 4;
  int niterations = 2;
  int nresults = nproc*npartners*niterations;
  int nparams = 2; 
  int njobs = 4;
  const char* param_names[] = { 
    "injection_bandwidth", 
    "network_bandwidth",
  };
  double** param_values = allocate_values(njobs, nparams);
  double** results = allocate_results(njobs, nresults);

  double inj_bws[] = { 1.0, 1.0 }; int nInj = sizeof(inj_bws) / sizeof(double);
  double net_bws[] = { 1.0, 1.0 }; int nNet = sizeof(net_bws) / sizeof(double);
  const char* units[] = { "GB/s", "GB/s" };

  int job = 0;
  int i,j;
  for (i=0; i < nInj; ++i){
    for (j=0; j < nNet; ++j, ++job){
      param_values[job][0] = inj_bws[i];
      param_values[job][1] = net_bws[j];
    }
  }

  void* queue = sstmac_uq_init(argc, argv);
  int max_nthread = 1;
  sstmac_uq_run_units(queue,
    njobs, nparams, nresults, max_nthread,
    param_names, param_values, units,
    results);

  for (job=0; job < njobs; ++job){
    debug_print("Job %d: {\n", job);
    for (i=0; i < nresults; ++i){
      double bw = results[job][i];
      debug_print("   %12.8fGB/s\n", bw);
      if (bw < 0.1 || bw > 1.0){
        printf("UQ test failed: got invalid bandwidths\n");
        return 1;
      }
    }
    debug_print("}\n");
  }
  printf("UQ test passed: got all valid bandwidths\n");

  return 0;
}

