
#ifdef __cplusplus
extern "C" {
#endif
void* sstmac_uq_init(int argc, char** argv);

void sstmac_uq_run(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], double* param_values[],
  double* results[]);

void sstmac_uq_finalize(void* queue);
#ifdef __cplusplus
}
#endif

