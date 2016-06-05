#include <sstmac/libraries/uq/uq.h>
#include <sstmac/main/driver.h>
#include <sprockit/errors.h>
#include <cstring>

using namespace sstmac;

extern "C" void*
sstmac_uq_init(int argc, char** argv, int* workerID)
{
  SimulationQueue* q = new SimulationQueue;
  q->init(argc, argv);
  *workerID = q->workerID();
  return q;
}

extern "C" void
sstmac_uq_busy_loop(void *queue)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  q->busyLoopMPI();
}

extern "C" void
sstmac_uq_finalize(void* queue)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  q->finalize();
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
wait_sims(Simulation** sims, int nsims, double** results, int nresults, uq_spawn_type_t spawn_ty)
{
  for (int i=0; i < nsims; ++i){
    if (spawn_ty == Fork) sims[i]->waitFork();
    else                  sims[i]->waitMPIScan();
    results[i] = sims[i]->results();
    if (sims[i]->numResults() != nresults){
      spkt_abort_printf("got wrong number of results for sim %d: expected %d, got %d",
        i, nresults, sims[i]->numResults());
    }
    delete sims[i];
  }
}

static const std::string&
set_param(sprockit::sim_parameters& params, const char* name, uq_param_t& p)
{
  switch(p.type){
   case ByteLength:
    return params[name].setByteLength(p.value, p.units);
    break;
   case Bandwidth:
    return params[name].setBandwidth(p.value, p.units);
    break;
   case Frequency:
    return params[name].setFrequency(p.value, p.units);
    break;
   case Latency:
   case Time:
    return params[name].setTime(p.value, p.units);
    break;
   case String:
    return params[name].set(p.cstr);
    break;
   case ValueWithUnits:
    return params[name].setValue(p.value, p.units);
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
  double* results[], uq_spawn_type_t ty)
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
    param_names, params, results, ty);
  free_params(params);
}

static Simulation*
send_scan_point(SimulationQueue* q,
  sprockit::sim_parameters& params,
  char* bufferPtr,
  int nparams,
  const char* param_names[],
  uq_param_t* param_vals)
{
  if (nparams > 128){
    spkt_throw_printf(sprockit::value_error,
       "too many params in UQ - max is 128");
  }

  *bufferPtr = (char) nparams; //should never be more than 256
  int total_size = 1;
  char* packPtr = bufferPtr + 1;
  for (int p=0; p < nparams; ++p){
    const std::string& val = set_param(params, param_names[p], param_vals[p]);
    int name_len = ::strlen(param_names[p]) + 1;
    int val_size = val.size() + 1;
    ::memcpy(packPtr, param_names[p], name_len);
    packPtr += name_len;
    ::memcpy(packPtr, val.c_str(), val_size);
    packPtr += val_size;
    total_size += name_len + val_size;
  }

  Simulation* sim = q->sendScanPoint(bufferPtr, nparams, total_size);
  return sim;
}

extern "C" void
sstmac_uq_run(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], uq_param_t* param_values[],
  double* results[], uq_spawn_type_t spawn_ty)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  Simulation** sims = new Simulation*[max_nthread];

  sprockit::sim_parameters params;

  int num_running = 0;
  int result_offset = 0;
  int last_job = njobs - 1;
  char* bufferPtr = 0;
  int entrySize = 64;
  int totalParamBufferSize = njobs*nparams*entrySize;
  int paramBufferSize = nparams*entrySize;
  if (spawn_ty == MPIScan){
    bufferPtr = new char[totalParamBufferSize];
  }

  for (int j=0; j < njobs; ++j){
    uq_param_t* param_vals = param_values[j];
    for (int param=0; param < nparams; ++param){
      set_param(params, param_names[param], param_vals[param]);
    }

    if (spawn_ty == Fork){
      sims[num_running++] = q->fork(params);
    }
    else if (spawn_ty == MPIScan){
      sims[num_running++] = send_scan_point(q, params, bufferPtr,
                                nparams, param_names, param_values[j]);
      bufferPtr += paramBufferSize;
    } else {
      spkt_throw_printf(sprockit::value_error,
         "invalid UQ spawn type %d", spawn_ty);
    }

    if (num_running == max_nthread || j == last_job){
      wait_sims(sims, num_running, results+result_offset, nresults, Fork);
      result_offset += num_running;
      num_running = 0;
    }
  }

  delete[] sims;
}


