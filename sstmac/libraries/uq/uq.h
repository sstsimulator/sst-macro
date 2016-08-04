
#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  ByteLength,
  Bandwidth,
  Latency,
  Time,
  Frequency,
  String,
  ValueWithUnits
} uq_param_type_t;

typedef enum {
  Fork,
  MPIScan
} uq_spawn_type_t;

typedef struct 
{
  double value;
  const char* units;
  const char* cstr;
  uq_param_type_t type;
} uq_param_t;
/**
 Allocate a 2D double array
 @param njobs
 @param nresults
*/
double** allocate_results(int njobs, int nresults);

/**
 Allocate a 2D double array
 @param njobs
 @param nparams
*/
double** allocate_values(int njobs, int nparams);

/**
 Allocate a 2D array of param structs
 @param njobs
 @param nparams
*/
uq_param_t** allocate_params(int njobs, int nparams);

int sstmac_uq_int_param(void* queue, const char* param);

int sstmac_uq_double_param(void* queue, const char* param);

int sstmac_uq_sim_nproc(void* queue);

int sstmac_uq_max_nproc(void* queue);

/**
 Free a 2D array allocated by allocate_results function
 @param results
*/
void free_results(double** results);

/**
 Free a 2D array allocated by allocate_values function
 @param values
*/
void free_values(double** values);

/**
 Free a 2D array allocated by allocate_params function
 @param params
*/
void free_params(uq_param_t** params);

/**
 @param argc The argc that would be used by a standalone SST/macro simulation
 @param argv The argv that would be used by a standalone SST/macro simulation
 @param workerID The ID of this worker in a parallel group. 0 return means master.
                 Workers who receiver other than zero should immediately go into
                 sstmac_uq_busy_loop and do nothing else.
 @return A void* pointer to the simulation queue object. This pointer
         should NOT be freed. Value is void* for C compatibility.
*/
void* sstmac_uq_init(int argc, char** argv, int* workerID);


void sstmac_uq_busy_loop(void* queue);

/**
 Run a set of jobs with particular parameters, forking new procs for parallelism
 @param queue A pointer to a queue object created by sstmac_uq_init
 @param njobs The number of jobs (simulations) to run
 @param nparams The number of parameters to set for each job
 @param nresults The number of results returned by each job
 @param max_nthread The maximum number of threads or, i.e.
                    the max number of jobs that can run simultaneously
 @param param_names  An array of size nparams. The name of each parameter
                     to configure for each job
 @param param_values A 2D array of size njobs X nparams
                     The value corresponding to each paramter for all jobs
                     Indexed as p[jobID][paramID]
 @param results      A 2D array of size njobs X nresults 
                     Will hold the result values for each job
                     Indexed as p[jobID][resultID]
 @param ty        The type of run to perform. Fork new procs or MPI scan.
*/
void sstmac_uq_run(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], uq_param_t* param_values[],
  double* results[], uq_spawn_type_t ty);

/**
 Run a set of jobs with particular parameters
 @param queue A pointer to a queue object created by sstmac_uq_init
 @param njobs The number of jobs (simulations) to run
 @param nparams The number of parameters to set for each job
 @param nresults The number of results returned by each job
 @param max_nthread The maximum number of threads or, i.e.
                    the max number of jobs that can run simultaneously
 @param param_names  An array of size nparams. The name of each parameter
                     to configure for each job
 @param param_values A 2D array of size njobs X nparams
                     The value corresponding to each paramter for all jobs
                     Indexed as p[jobID][paramID]
 @param results      A 2D array of size njobs X nresults 
                     Will hold the result values for each job
                     Indexed as p[jobID][resultID]
*/
void sstmac_uq_run_units(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], double* param_values[], const char* units[],
  double* results[], uq_spawn_type_t spwan_ty);

/**
 @param queue A pointer to a queue object created by sstmac_uq_init
              After finalize, the queue is no longer usable and all
              resources used by the queue are freed
*/
void sstmac_uq_finalize(void* queue);
#ifdef __cplusplus
}
#endif

