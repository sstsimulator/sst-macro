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

namespace sstmac {

double* SimulationQueue::results_ = 0;
int SimulationQueue::num_results_ = 0;


void
Simulation::setParameters(sprockit::sim_parameters *params)
{
  params->combine_into(&params_);
}

void
Simulation::wait()
{
  int status;
  pid_t result = waitpid(pid_, &status, 0);
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
      write(pfd[WRITE], results_, num_results_);
    close(pfd[WRITE]);
    exit(0);
    return 0;
  } else {
    close(pfd[WRITE]);
    Simulation* sim = new Simulation;
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
        sim_stats stats;
        int bytes = read(sim->readPipe(), &stats, sizeof(sim_stats));
        if (bytes <= 0){
          spkt_throw(sprockit::value_error,
               "failed reading pipe from simulation");
        }
        if (stats.numResults){
          double* results = new double[stats.numResults];
          bytes = read(sim->readPipe(), results, stats.numResults*sizeof(double));
          sim->setResults(results, stats.numResults);
        }
        close(sim->readPipe());
        sim->setSimulatedTime(stats.simulatedTime);
        sim->setWallTime(stats.wallTime);
        pending_.erase(it);
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
