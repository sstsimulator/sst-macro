/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef sstmac_libraries_uq_uq_h
#define sstmac_libraries_uq_uq_h

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
double** allocate_results(void* queue, int njobs, int nresults);

/**
 Allocate a 2D double array
 @param njobs
 @param nparams
*/
double** allocate_values(void* queue, int njobs, int nparams);

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

void sstmac_uq_stop(void* queue);

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

#endif