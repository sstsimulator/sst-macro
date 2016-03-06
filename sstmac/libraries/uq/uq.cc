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

template <class T>
void
tmpl_free_values(T** vals)
{
  delete[] vals[0];
  delete[] vals;
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

double**
allocate_values(int nrows, int ncols)
{
  return tmpl_allocate_values<double>(nrows, ncols);
}

double**
allocate_results(int nrows, int ncols)
{
  return tmpl_allocate_values<double>(nrows, ncols);
}

uq_param_t**
allocate_params(int nrows, int ncols)
{
  return tmpl_allocate_values<uq_param_t>(nrows, ncols);
}

void
free_params(uq_param_t** params)
{
  tmpl_free_values<uq_param_t>(params);
}

void
free_results(double** results)
{
  tmpl_free_values<double>(results);
}

void
free_values(double** results)
{
  tmpl_free_values<double>(results);
}

static void 
wait_sims(Simulation** sims, int nsims, double** results, int nresults)
{
  for (int i=0; i < nsims; ++i){
    sims[i]->wait();
    results[i] = sims[i]->results();
    if (sims[i]->numResults() != nresults){
      spkt_abort_printf("got wrong number of results for sim %d: expected %d, got %d",
        i, nresults, sims[i]->numResults());
    }
    delete sims[i];
  }
}

static void
set_param(sprockit::sim_parameters& params, const char* name, uq_param_t& p)
{
  switch(p.type){
   case ByteLength:
    params[name].setByteLength(p.value, p.units);
    break;
   case Bandwidth:
    params[name].setBandwidth(p.value, p.units);
    break;
   case Frequency:
    params[name].setFrequency(p.value, p.units);
    break;
   case Latency:
   case Time:
    params[name].setTime(p.value, p.units);
    break;
   case String:
    params[name].set(p.cstr);
    break;
   case ValueWithUnits:
    params[name].setValue(p.value, p.units);
    break;
   default:
    spkt_abort_printf("invalid paramter type - make sure param.type is initialized");
  }
}

extern "C" void
sstmac_uq_run_units(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], double* param_values[],
  const char* param_units[],
  double* results[])
{
  uq_param_t** params = allocate_params(njobs, nparams);
  for (int j=0; j < njobs; ++j){
    for (int p=0; p < nparams; ++p){
      params[j][p].value = param_values[j][p];
      params[j][p].type = ValueWithUnits;
      params[j][p].units = param_units[p];
    }
  }
  sstmac_uq_run(queue, njobs, nparams, nresults, max_nthread,
    param_names, params, results);
  free_params(params);
}

extern "C" void
sstmac_uq_run(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], uq_param_t* param_values[],
  double* results[])
{
  SimulationQueue* q = (SimulationQueue*) queue;
  sprockit::sim_parameters params;
  Simulation* sims[max_nthread];

  int num_running = 0;

  for (int j=0; j < njobs; ++j){
    if (num_running == max_nthread){
      int result_offset = j - max_nthread;
      wait_sims(sims, max_nthread, results+result_offset, nresults);
      num_running = 0;
    }
    uq_param_t* param_vals = param_values[j];
    for (int param=0; param < nparams; ++param){
      set_param(params, param_names[param], param_vals[param]);
    }
    sims[num_running++] = q->fork(params);
  }
  int result_offset = njobs - num_running;
  wait_sims(sims, num_running, results+result_offset, nresults);
}


