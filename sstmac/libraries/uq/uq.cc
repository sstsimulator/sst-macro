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

#include <sstmac/libraries/uq/uq.h>
#include <sstmac/main/driver.h>
#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <cstring>
#include <vector>
#include <sstmac/software/launch/launch_request.h>

using namespace sstmac;

MakeDebugSlot(uq);
MakeDebugSlot(uq_sanity);

extern "C" int
sstmac_uq_int_param(void* queue, const char* param)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  return q->template_params()->get_int_param(param);
}

extern "C" int
sstmac_uq_double_param(void* queue, const char* param)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  return q->template_params()->get_double_param(param);
}

extern "C" int
sstmac_uq_max_nproc(void* queue)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  return q->maxParallelWorkers();
}

extern "C" int
sstmac_uq_sim_nproc(void* queue)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  sprockit::sim_parameters* src_params = q->template_params();
  sprockit::sim_parameters* src_app_params = src_params->get_optional_namespace("app1");
  sprockit::sim_parameters params;
  src_app_params->combine_into(&params);
  if (src_params->has_param("launch_app1_cmd")){
    params["launch_cmd"] = src_params->get_param("launch_app1_cmd");
  } 
  if (src_params->has_param("launch_app1_size")){
    params["size"] = src_params->get_param("launch_app1_size");
  }
  int nproc, procs_per_node;
  std::vector<int> affinities;
  sstmac::sw::app_launch_request::parse_launch_cmd(&params, nproc, procs_per_node, affinities);
  return nproc;
}

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
allocate_values(void* queue, int nrows, int ncols)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  return q->allocateParams(nrows, ncols);
}

double**
allocate_results(void* queue, int nrows, int ncols)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  return q->allocateResults(nrows, ncols);
}

uq_param_t**
allocate_params(void* queue, int nrows, int ncols)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  return q->allocateParamStructs(nrows, ncols);
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

    double avg = 0;
    for (int j=0; j < nresults; ++j){
      avg += results[i][j];
    }
    avg /= nresults;
    debug_printf(sprockit::dbg::uq_sanity,
      "Job %d: %12.8f", i, avg);
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
  uq_param_t** params = allocate_params(queue, njobs, nparams);
  for (int j=0; j < njobs; ++j){
    for (int p=0; p < nparams; ++p){
      params[j][p].value = param_values[j][p];
      params[j][p].type = ValueWithUnits;
      params[j][p].units = param_units[p];
    }
  }
  sstmac_uq_run(queue, njobs, nparams, nresults, max_nthread,
    param_names, params, results, ty);
}

static Simulation*
send_scan_point(SimulationQueue* q,
  sprockit::sim_parameters& params,
  char* bufferPtr,
  int nparams,
  double* resultPtr,
  int nresults,
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

  Simulation* sim = q->sendScanPoint(total_size, bufferPtr, nresults, resultPtr);
  return sim;
}

extern "C" void
sstmac_uq_stop(void* queue)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  q->teardown();
}

extern "C" void
sstmac_uq_run(void* queue,
  int njobs, int nparams, int nresults, int max_nthread,
  const char* param_names[], uq_param_t* param_values[],
  double* results[], uq_spawn_type_t spawn_ty)
{
  SimulationQueue* q = (SimulationQueue*) queue;
  if (max_nthread <= 0) max_nthread = q->maxParallelWorkers();

  Simulation** sims = q->allocateSims(max_nthread);
  char* bufferPtr = nullptr;
  int num_running = 0;
  int result_offset = 0;
  int last_job = njobs - 1;
  int entrySize = 64;
  int totalParamBufferSize = njobs*nparams*entrySize;
  int paramBufferSize = nparams*entrySize;

  if (spawn_ty == MPIScan){
    bufferPtr = q->allocateTmpBuffer(totalParamBufferSize);
  }

  q->buildUp();

  sprockit::sim_parameters params;


  for (int j=0; j < njobs; ++j){
    uq_param_t* param_vals = param_values[j];
    for (int param=0; param < nparams; ++param){
      set_param(params, param_names[param], param_vals[param]);
    }

    if (spawn_ty == Fork){
      sims[num_running++] = q->fork(params, nresults, results[j]);
    }
    else if (spawn_ty == MPIScan){
      sims[num_running++] = send_scan_point(q, params, bufferPtr,
                                nparams, results[j], nresults, param_names, param_values[j]);
      bufferPtr += paramBufferSize;
    } else {
      spkt_throw_printf(sprockit::value_error,
         "invalid UQ spawn type %d", spawn_ty);
    }

    if (num_running == max_nthread || j == last_job){
      wait_sims(sims, num_running, results+result_offset, nresults, spawn_ty);
      debug_printf(sprockit::dbg::uq,
                   "Finished through simulation point %d", j);
      result_offset += num_running;
      num_running = 0;
    }
  }
}