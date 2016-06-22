#ifndef sstmac_main_DRIVER_H
#define sstmac_main_DRIVER_H

#include <sprockit/sim_parameters.h>
#include <sstmac/backends/native/manager_fwd.h>
#include <sstmac/main/sstmac.h>
#include <list>
#include <iostream>

#ifdef SSTMAC_MPI_DRIVER
#include <mpi.h>
#endif

namespace sstmac {

template <class T>
class Labeler {
 public:
  static int assign_label(char* buffer, const T& t){
    T* tPtr = reinterpret_cast<T*>(buffer);
    *tPtr = t;
    return sizeof(T);
  }

  static int extract_label(char* buffer, T& t){
    T* tPtr = reinterpret_cast<T*>(buffer);
    t = *tPtr;
    return sizeof(T);
  }
};

template <>
class Labeler<const char*> {
 public:
  static int assign_label(char* buffer, const char* msg){
   const char** tPtr = reinterpret_cast<const char**>(buffer);
   *tPtr = msg;
   return sizeof(const char*);
  }

  static int extract_label(char* buffer, const char*& msg){
   const char** tPtr = reinterpret_cast<const char**>(buffer);
   msg = *tPtr;
   return sizeof(const char*);
  }
};

template <class T>
int
append_label(char* buffer, const T& t){
  return Labeler<T>::assign_label(buffer, t);
}

template <class T>
int
extract_label(char* buffer, T& t){
  return Labeler<T>::extract_label(buffer, t);
}


typedef int pipe_t[2];

class Simulation
{
  friend class SimulationQueue;

 public:
  Simulation() : 
    complete_(false),
    results_(0)
  {
  }

  Simulation(int nresults) :
    complete_(false),
    results_(new double[nresults])
  {
  }

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
    results_ = results;
    stats_.numResults = numResults;
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

  Simulation*
  fork(sprockit::sim_parameters& params){
    return fork(&params);
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

  void init(int argc, char** argv);

  void finalize();

  Simulation*
  fork(sprockit::sim_parameters* params);

  Simulation*
  waitForForked();

  void
  clear(Simulation* sim);

  void
  run(sprockit::sim_parameters* params, sim_stats& stats);

  static void
  publishResults(double* results, int nresults);

  Simulation*
  sendScanPoint(char* bufferPtr, int nparams, int totalSize);

  void
  rerun(sprockit::sim_parameters* params, sim_stats& stats);

  void busyLoopMPI();

  void runScanPoint(char* buffer, sim_stats& stats);

  int workerID() const {
    return me_;
  }

 private:
  std::list<Simulation*> pending_;
  parallel_runtime* rt_;
  sprockit::sim_parameters template_params_;
  opts template_opts_;
  static double* results_;
  static int num_results_;

 private:
  int nproc_;
  int me_;
  int next_worker_;
  bool first_run_;
};

}


#endif // DRIVER_H

