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

#include <sstmac/main/driver.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/backends/native/manager.h>
#include <sprockit/errors.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#if SSTMAC_MPI_DRIVER
#include <mpi.h>
#endif

#define READ 0
#define WRITE 1

MakeDebugSlot(driver)

namespace sstmac {

static sprockit::need_delete_statics<SimulationQueue> del_statics;

double* SimulationQueue::results_ = nullptr;
int SimulationQueue::num_results_ = 0;

static int results_tag = 42;
static int init_tag = 43;
static int stats_tag = 44;
static int terminate_tag = 45;

#define driver_debug(...) \
  debug_printf(sprockit::dbg::driver, __VA_ARGS__)

void
Simulation::setParameters(sprockit::sim_parameters *params)
{
  params->combine_into(&params_);
}

Simulation::~Simulation()
{
  if (allocated_results_)
    delete[] results_;
}

void
Simulation::waitFork()
{
  if (complete_){
    driver_debug("forked process %d already complete", pid_);
    return;
  }

  int status;
  driver_debug("start wait on pid=%d", pid_);
  pid_t result = waitpid(pid_, &status, 0);
  driver_debug("finished wait on pid=%d", pid_);
  finalize();
}

void
Simulation::finalize()
{
  int bytes = read(readPipe(), &stats_, sizeof(sim_stats));
  if (bytes <= 0){
    spkt_throw(sprockit::value_error,
         "failed reading pipe from simulation");
  }
  if (stats_.numResults){
    if (!results_){
      allocated_results_ = true;
      results_ = new double[stats_.numResults];
    }
    bytes = read(readPipe(), results_, stats_.numResults*sizeof(double));
    driver_debug("finalize nresults=%d", stats_.numResults);
  }
  close(readPipe());

  complete_ = true;
}

void
Simulation::waitMPIScan()
{
#if SSTMAC_MPI_DRIVER
  if (complete_) return;

  driver_debug("master waiting for simulation to complete");
  MPI_Waitall(3, mpi_requests_, MPI_STATUSES_IGNORE);
  driver_debug("received all results from simulation - now complete");
  complete_ = true;
#else
  spkt_throw(sprockit::unimplemented_error,
    "Simulation::waitMPIScan()");
#endif
}


SimulationQueue::SimulationQueue() :
 first_run_(true),
 next_worker_(0), //starts from 1
 me_(0),
 nproc_(1),
 tmp_buffer_(nullptr),
 sims_(nullptr),
 tmp_results_(nullptr),
 tmp_params_(nullptr),
 tmp_structs_(nullptr),
 tmp_buf_size_(0),
 built_up_(false),
 nsims_(0),
 result_buf_size_(0,0),
 param_buf_size_(0,0)
{
}

void
SimulationQueue::delete_statics()
{
  if (results_) delete[] results_;
}


template <class T>
T**
tmpl_allocate_values(int nrows, int ncols)
{
  T* vals = new T[nrows*ncols];
  T** ptrs = new T*[nrows];
  T* ptr = vals;
  for (int i=0; i < nrows; ++i, ptr += ncols){
    ptrs[i] = ptr;
  }
  return ptrs;
}

template <class T>
void
tmpl_free_values(T** vals)
{
  T* valarray = vals[0];
  delete[] valarray;
  delete[] vals;
}

SimulationQueue::~SimulationQueue()
{
  if (sims_)        delete[] sims_;
  if (tmp_buffer_)  delete[] tmp_buffer_;
  if (tmp_results_) tmpl_free_values(tmp_results_);
  if (tmp_params_)  tmpl_free_values(tmp_params_);
  if (tmp_structs_) tmpl_free_values(tmp_structs_);
}

uq_param_t**
SimulationQueue::allocateParamStructs(int njobs, int nparams)
{
  int& my_njobs = struct_buf_size_.first;
  int& my_nparams = struct_buf_size_.second;
  if (tmp_structs_){
    if (my_njobs >= njobs && my_nparams >= nparams){
      return tmp_structs_;
    } else {
      tmpl_free_values(tmp_structs_);
      tmp_structs_ = nullptr;
    }
  }
  tmp_structs_ = tmpl_allocate_values<uq_param_t>(njobs, nparams);
  my_njobs = njobs;
  my_nparams = nparams;
  return tmp_structs_;
}

double**
SimulationQueue::allocateResults(int njobs, int nresults)
{
  int& my_njobs = result_buf_size_.first;
  int& my_nresults = result_buf_size_.second;
  if (tmp_results_){
    if (my_njobs >= njobs && my_nresults >= nresults){
      return tmp_results_;
    } else {
      tmpl_free_values(tmp_results_);
      tmp_results_ = nullptr;
    }
  }
  tmp_results_ = tmpl_allocate_values<double>(njobs, nresults);
  my_njobs = njobs;
  my_nresults = nresults;
  return tmp_results_;
}

double**
SimulationQueue::allocateParams(int njobs, int nparams)
{
  int& my_njobs = param_buf_size_.first;
  int& my_nparams = param_buf_size_.second;
  if (tmp_params_){
    if (my_njobs >= njobs && my_nparams >= nparams){
      return tmp_params_;
    } else {
      tmpl_free_values(tmp_params_);
      tmp_params_ = nullptr;
    }
  }
  tmp_params_ = tmpl_allocate_values<double>(njobs, nparams);
  my_njobs = njobs;
  my_nparams = nparams;
  return tmp_params_;
}


Simulation**
SimulationQueue::allocateSims(int max_nthread)
{
  if (sims_){ 
    if (nsims_ >= max_nthread){
      return sims_;
    } else {
      //not enough
      delete[] sims_;
      sims_ = 0;
    }
  } 
  sims_ = new Simulation*[max_nthread];
  nsims_ = max_nthread;
  return sims_;
}

char*
SimulationQueue::allocateTmpBuffer(size_t size)
{
  if (tmp_buffer_){
    if (tmp_buf_size_ >= size){
      return tmp_buffer_;
    } else {
      delete [] tmp_buffer_;
      tmp_buffer_ = 0;
    }
  }
  tmp_buffer_ = new char[size];
  tmp_buf_size_ = size;
  return tmp_buffer_;
}

void
SimulationQueue::teardown()
{
  if (!built_up_)
    return;

#if SSTMAC_MPI_DRIVER
  char buffer[1];
  for (int i=1; i < nproc_; ++i){
    MPI_Send(buffer, 1, MPI_INT, i, terminate_tag, MPI_COMM_WORLD);
  }
#endif
  built_up_ = false;
}


double*
SimulationQueue::allocateResults(int nresults)
{
  if (results_){
    if (num_results_ >= nresults){
      //already good
      num_results_ = nresults;
      return results_;
    }
    delete [] results_;
    results_ = nullptr;
  }

  results_ = new double[nresults];
  num_results_ = nresults;
  return results_;
}

void
SimulationQueue::clear(Simulation *sim)
{
  delete sim;
  std::list<Simulation*>::iterator it, end = pending_.end();
  for (it=pending_.begin(); it != end; ++it){
    Simulation* test = *it;
    if (test == sim){
      pending_.erase(it);
      break;
    }
  }
  delete sim;
}

void
SimulationQueue::run(sprockit::sim_parameters* params, sim_stats& stats)
{
  template_params_.combine_into(params, false, false/*no overwrite*/, true);
  sstmac::remap_params(params, false /* not verbose */);
  ::sstmac::run(template_opts_, rt_, params, stats);
}

Simulation*
SimulationQueue::fork(sprockit::sim_parameters* params, int nresults, double* resultPtr)
{
  template_params_.combine_into(params, false, false, true);
  pipe_t pfd;
  if (pipe(pfd) == -1){
    fprintf(stderr, "failed opening pipe\n");
    abort();
  }

  pid_t pid = ::fork();

  if (pid == 0){
    sim_stats stats;
    run(params, stats);
    stats.numResults = num_results_;
    close(pfd[READ]);
    write(pfd[WRITE], &stats, sizeof(sim_stats));
    if (results_)
      write(pfd[WRITE], results_, num_results_*sizeof(double));
    close(pfd[WRITE]);
    exit(0);
    return 0;
  } else {
    close(pfd[WRITE]);
    Simulation* sim = new Simulation;
    sim->setResults(resultPtr, nresults);
    driver_debug("forked process %d", pid);
    sim->setPid(pid);
    sim->setParameters(params);
    sim->setPipe(pfd);
    pending_.push_back(sim);
    return sim;
  }
}


Simulation*
SimulationQueue::waitForForked()
{
  while (1){
    std::list<Simulation*>::iterator it, end = pending_.end();
    for (it=pending_.begin(); it != end; ++it){
      Simulation* sim = *it;
      int status;
      pid_t result = waitpid(sim->pid(), &status, WNOHANG);
      if (result > 0){
        driver_debug("waited on process %d", sim->pid());
        pending_.erase(it);
        sim->finalize();
        return sim;
      }
    }
  }
}

void
SimulationQueue::init(int argc, char** argv)
{
#if SSTMAC_MPI_DRIVER
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc_);
  MPI_Comm_rank(MPI_COMM_WORLD, &me_);
  next_worker_ = 1%nproc_;
#endif
  //set up the search path
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_INSTALL_INCLUDE_PATH);
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_SRC_INCLUDE_PATH);
  rt_ = ::sstmac::init();
  init_opts(template_opts_, argc, argv);
  init_params(rt_, template_opts_, &template_params_, true);
  if (sprockit::debug::slot_active(sprockit::dbg::driver)){
    template_params_.print_params();
  }
}

void
SimulationQueue::finalize()
{
  if (built_up_)
    teardown();
  ::sstmac::finalize(rt_);
#if SSTMAC_MPI_DRIVER
  MPI_Finalize();
#endif
}

void
SimulationQueue::busyLoopMPI()
{
#if SSTMAC_MPI_DRIVER
  while (1){
    int me; MPI_Comm_rank(MPI_COMM_WORLD, &me);
    char paramBuffer[4096];
    MPI_Status stat;
    int master = 0;
    driver_debug("worker %d waiting on for new job from master", me);
    MPI_Recv(paramBuffer, 4096, MPI_CHAR, master, MPI_ANY_TAG, MPI_COMM_WORLD, &stat);
    driver_debug("received buffer on tag %d on worker %d", stat.MPI_TAG, me);
    if (stat.MPI_TAG == terminate_tag){
      return;
    } else {
      sim_stats stats;
      runScanPoint(paramBuffer, stats);
      MPI_Request reqs[2];
      MPI_Isend(&stats, sizeof(sim_stats), MPI_BYTE, master, stats_tag, MPI_COMM_WORLD, &reqs[0]);
      MPI_Isend(results_, num_results_, MPI_DOUBLE, master, results_tag, MPI_COMM_WORLD, &reqs[1]);
      driver_debug("worker %d waiting on send results to master", me);
      MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
    }
  }
#else
  spkt_throw(sprockit::unimplemented_error,
    "Simulation::busyLoopMPI()");
#endif
}

Simulation*
SimulationQueue::sendScanPoint(int bufferSize, char *bufferPtr, int nresults, double* resultPtr)
{
#if SSTMAC_MPI_DRIVER
  driver_debug("sending scan point with buffer size=%d nresults=%d to worker %d",
    bufferSize, nresults, next_worker_);
  

  Simulation* sim = new Simulation;
  if (resultPtr) sim->setResults(resultPtr, nresults);
  else sim->allocateResults(nresults);

  if (runJobsOnMaster() && next_worker_ == me_)
    setNextWorker();
 
  if (next_worker_ == me_){
    sim_stats stats;
    //i have looped around - use me in running jobs
    runScanPoint(bufferPtr, stats);
    sim->setStats(stats);
    if (num_results_ != nresults){
      spkt_throw_printf(sprockit::value_error,
        "got wrong number of results from simulation queue: %d != %d",
        num_results_, nresults);

    }
    ::memcpy(sim->results(), results_, num_results_*sizeof(double));
    sim->setComplete(true);
  } else {
    driver_debug("sending buffer of size %d on tag %d to worker %d",
                  bufferSize, init_tag, next_worker_);
    MPI_Isend(bufferPtr, bufferSize, MPI_CHAR, next_worker_, init_tag,
              MPI_COMM_WORLD, sim->initSendRequest());
    driver_debug("receiving stats on tag %d from worker %d",
                  stats_tag, next_worker_);
    MPI_Irecv(sim->stats(), sizeof(sim_stats), MPI_BYTE, next_worker_, stats_tag,
              MPI_COMM_WORLD, sim->recvStatsRequest());
    driver_debug("receiving %d results on tag %d from worker %d",
                  nresults, results_tag, next_worker_);
    MPI_Irecv(sim->results(), nresults, MPI_DOUBLE, next_worker_, results_tag,
              MPI_COMM_WORLD, sim->recvResultsRequest());
  }

  setNextWorker();
  return sim;
#else
  spkt_throw(sprockit::unimplemented_error,
    "Simulation::sendScanPoint()");
#endif
}

void
SimulationQueue::runScanPoint(char* buffer, sim_stats& stats)
{
#if SSTMAC_MPI_DRIVER
  //first char is number of params
  char nparams = *buffer;
  char* bufferPtr = buffer + 1;
  sprockit::sim_parameters params;
  template_params_.combine_into(&params, false, false /*no overwrite*/, true);
  for (int i=0; i < nparams; ++i){
    const char* param_name = bufferPtr;
    int name_len = ::strlen(bufferPtr) + 1; //null char
    bufferPtr += name_len;
    const char* param_val = bufferPtr;
    int val_len = ::strlen(bufferPtr) + 1; //+1 null char
    bufferPtr += val_len;
    driver_debug("adding parameters %s = %s", 
      param_name, param_val);
    params[param_name] = param_val;
  }
  rerun(&params, stats);
  driver_debug("got stats with %d results", stats.numResults);
#else
  spkt_throw(sprockit::unimplemented_error,
    "Simulation::runScanPoint()");
#endif
}

void
SimulationQueue::rerun(sprockit::sim_parameters* params, sim_stats& stats)
{
  sstmac::remap_params(params, false /*not verbose*/);
  sstmac::env::params = params;
  if (first_run_){
    ::sstmac::init_first_run(rt_, params);
    first_run_ = false;
  }
  ::sstmac::run_params(rt_, params, stats);
  stats.numResults = num_results_;
}

}