#include <sstmac/main/driver.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/errors.h>
#include <sprockit/fileio.h>

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define READ 0
#define WRITE 1

MakeDebugSlot(driver)

namespace sstmac {

double* SimulationQueue::results_ = 0;
int SimulationQueue::num_results_ = 0;

#define driver_debug(...) \
  debug_printf(sprockit::dbg::driver, __VA_ARGS__)

void
Simulation::setParameters(sprockit::sim_parameters *params)
{
  params->combine_into(&params_);
}

void
Simulation::wait()
{
  if (complete_)
    return;

  int status;
  driver_debug("wait on pid=%d", pid_);
  pid_t result = waitpid(pid_, &status, 0);
  finalize();
}

void
Simulation::finalize()
{
  sim_stats stats;
  int bytes = read(readPipe(), &stats, sizeof(sim_stats));
  if (bytes <= 0){
    spkt_throw(sprockit::value_error,
         "failed reading pipe from simulation");
  }
  if (stats.numResults){
    double* results = new double[stats.numResults];
    bytes = read(readPipe(), results, stats.numResults*sizeof(double));
    setResults(results, stats.numResults);
    driver_debug("finalize nresults=%d", num_results_);
  }
  close(readPipe());
  setSimulatedTime(stats.simulatedTime);
  setWallTime(stats.wallTime);

  complete_ = true;
}

void
SimulationQueue::publishResults(double* results, int nresults)
{
  results_ = results;
  num_results_ = nresults;
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
  params->combine_into(&template_params_);
  sstmac::process_init_params(&template_params_);
  sstmac::remap_deprecated_params(&template_params_);
  ::sstmac::run(template_opts_, rt_, &template_params_, stats, false/*not just params*/);
}

Simulation*
SimulationQueue::fork(sprockit::sim_parameters* params)
{
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
    driver_debug("forked process %d", pid);
    sim->setPid(pid);
    sim->setParameters(&template_params_);
    sim->setPipe(pfd);
    pending_.push_back(sim);
    return sim;
  }
}

Simulation*
SimulationQueue::waitForCompleted()
{
  while (1){
    std::list<Simulation*>::iterator it, end = pending_.end();
    for (it=pending_.begin(); it != end; ++it){
      Simulation* sim = *it;
      int status;
      pid_t result = waitpid(sim->pid(), &status, WNOHANG);
      if (result > 0){
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
  //set up the search path
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_INSTALL_INCLUDE_PATH);
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_SRC_INCLUDE_PATH);
  rt_ = ::sstmac::init();
  init_opts(template_opts_, argc, argv);
  init_params(rt_, template_opts_, &template_params_, true);
}

void
SimulationQueue::finalize()
{
  ::sstmac::finalize(rt_);
}

}
