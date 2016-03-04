#include <sstmac/libraries/uq/uq.h>
#include <sstmac/main/driver.h>
#include <sprockit/errors.h>

using namespace sstmac;

extern "C" void*
sstmac_uq_init(int argc, char** argv)
{
  SimulationQueue* q = new SimulationQueue;
  q->init(argc, argv);
  return q;
}

extern "C" void
sstmac_uq_finalize(void* queue)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  delete q;
}

static void 
wait_sims(Simulation** sims, int nsims, double** results, int nresults)
{
  for (int i=0; i < nsims; ++i){
    sims[i]->wait();    
    results[i] = sims[i]->results();
    if (sims[i]->numResults() != nresults){
      spkt_abort_printf("got wrong number of results: expected %d, got %d",
        nresults, sims[i]->numResults());
    }
    delete sims[i];
  }
}

extern "C" void
sstmac_uq_run(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], double* param_values[],
  double* results[])
{
  SimulationQueue* q = (SimulationQueue*) queue;
  sprockit::sim_parameters params;
  Simulation* sims[max_nthread];

  int num_running = 0;

  for (int j=0; j < njobs; ++j, ++num_running){
    if (num_running == max_nthread){
      int result_offset = j - max_nthread;
      wait_sims(sims, max_nthread, results+result_offset, nresults);
      num_running = 0;
    }
    double* param_vals = param_values[j];
    for (int param=0; param < nparams; ++param){
      params[param_names[param]] = param_vals[param];
    }
    sims[num_running++] = q->fork(params);
  }
  int result_offset = njobs - num_running;
  wait_sims(sims, num_running, results+result_offset, nresults);
}


