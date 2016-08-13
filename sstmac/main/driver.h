#ifndef sstmac_main_DRIVER_H
#define sstmac_main_DRIVER_H

#include <sprockit/sim_parameters.h>
#include <sstmac/backends/native/manager_fwd.h>
#include <sstmac/main/sstmac.h>
#include <sstmac/common/sstmac_config.h>
#include <list>
#include <iostream>
#include <sstmac/libraries/uq/uq.h>

#ifdef SSTMAC_MPI_DRIVER
#include <mpi.h>
#endif

namespace sstmac {

typedef int pipe_t[2];

class Simulation
{
  friend class SimulationQueue;

 public:
  Simulation() : 
    complete_(false),
    results_(nullptr),
    allocated_results_(false)
  {
  }

  ~Simulation();

  double
  wallTime() const {
    return stats_.wallTime;
  }

  double
  simulatedTime() const {
    return stats_.simulatedTime;
  }

  sim_stats*
  stats() {
    return &stats_;
  }

  void finalize();

  void waitFork();

  pid_t
  pid() const {
    return pid_;
  }

  void
  setResults(double* results, int numResults){
    if (allocated_results_)
      delete[] results_;

    results_ = results;
    stats_.numResults = numResults;
    allocated_results_ = false;
  }

  void
  allocateResults(int nresults){
    if (allocated_results_ && stats_.numResults >= nresults){
      //do nothing - good already
    } else {
      allocated_results_ = true;
      results_ = new double[nresults];
    }
    stats_.numResults = nresults;
  }

  void
  setStats(const sim_stats& stats) {
    stats_ = stats;
  }

  double*
  results() const {
    return results_;
  }


  bool
  complete() const {
    return complete_;
  }

  void
  setComplete(bool flag){
    complete_ = flag;
  }

  int
  numResults() const {
    return stats_.numResults;
  }

  void
  setLabel(int idx){
    idx_ = idx;
  }

  int
  label() const {
    return idx_;
  }

 private:
  void
  setPid(pid_t pid){
    pid_ = pid;
  }

  void
  setPipe(pipe_t p){
    pfd_[0] = p[0];
    pfd_[1] = p[1];
  }

  int
  readPipe() const {
    return pfd_[0];
  }

  int
  writePipe() const {
    return pfd_[1];
  }

  void
  setParameters(sprockit::sim_parameters* params);

  sprockit::sim_parameters params_;
  sim_stats stats_;
  char label_[256];
  int label_offset_;
  pid_t pid_;
  pipe_t pfd_;
  double* results_;
  bool complete_;
  int idx_;
  bool allocated_results_;


 public:
  void waitMPIScan();

#if SSTMAC_MPI_DRIVER
 public:
  MPI_Request*
  initSendRequest() {
    return mpi_requests_;
  }

  MPI_Request*
  recvResultsRequest() {
    return &mpi_requests_[2];
  }

  MPI_Request*
  recvStatsRequest() {
    return &mpi_requests_[1];
  }

 private:
  MPI_Request mpi_requests_[3];
#endif

};


class SimulationQueue
{
 public:
  SimulationQueue();

  ~SimulationQueue();

  Simulation*
  fork(sprockit::sim_parameters& params, 
    int nresults = 0, 
    double* resultPtr = nullptr){
    return fork(&params, nresults, resultPtr);
  }

  bool
  runJobsOnMaster() const {
    return nproc_ <= 4;
  }

  int
  maxParallelWorkers() const {
    if (runJobsOnMaster()) return nproc_;
    else return nproc_ - 1;
  }

  void
  setNextWorker(){
    next_worker_ = (next_worker_ + 1) % nproc_;
  }

  void teardown();

  void buildUp(){
    built_up_ = true;
  }

  void init(int argc, char** argv);

  void finalize();

  Simulation*
  fork(sprockit::sim_parameters* params,
    int nresults = 0, 
    double* resultPtr = nullptr);

  Simulation*
  waitForForked();

  void
  clear(Simulation* sim);

  void
  run(sprockit::sim_parameters* params, sim_stats& stats);

  static double*
  allocateResults(int nresults);

  static void
  publishResults(){}

  static void
  delete_statics();

  Simulation*
  sendScanPoint(int bufferSize, char* bufferPtr, int nresults, double* resultPtr = nullptr);

  sprockit::sim_parameters*
  template_params() {
    return &template_params_;
  }

  void
  rerun(sprockit::sim_parameters* params, sim_stats& stats);

  void busyLoopMPI();

  void runScanPoint(char* buffer, sim_stats& stats);

  int workerID() const {
    return me_;
  }

  Simulation**
  allocateSims(int max_nthread);

  char*
  allocateTmpBuffer(size_t buf_size);

  double**
  allocateResults(int njobs, int nresults);

  double**
  allocateParams(int njobs, int nparams);

  uq_param_t**
  allocateParamStructs(int njobs, int nparams);


 private:
  bool built_up_;
  std::list<Simulation*> pending_;
  parallel_runtime* rt_;
  sprockit::sim_parameters template_params_;
  opts template_opts_;
  static double* results_;
  static int num_results_;
  Simulation** sims_;
  int nsims_;
  char* tmp_buffer_;
  size_t tmp_buf_size_;

  std::pair<int,int> result_buf_size_;
  std::pair<int,int> param_buf_size_;
  std::pair<int,int> struct_buf_size_;

  double** tmp_results_;
  double** tmp_params_;
  uq_param_t** tmp_structs_;

 private:
  int nproc_;
  int me_;
  int next_worker_;
  bool first_run_;
};

}


#endif // DRIVER_H

