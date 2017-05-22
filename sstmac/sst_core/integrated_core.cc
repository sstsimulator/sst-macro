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

#include <sstmac/common/sstmac_config.h>
#include <sstmac/main/sstmac.h>

#include <sst/core/element.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/pisces/pisces_nic.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_simple_network.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/hardware/node/simple_node.h>
#include <sstmac/hardware/topology/topology.h>

#include <vector>

#include <sprockit/util.h>
#include <sprockit/unordered.h>
#include <sprockit/debug.h>
#include <sprockit/output.h>

#include <Python.h>

#include <stdio.h>
#include <stddef.h>

static char py_sstmacro[] = {
#include "sstmacro.inc"
    0x00};

using namespace sstmac;
using namespace SST;

sprockit::sim_parameters*
make_spkt_params_from_sst_params(SST::Params& map)
{
  sprockit::sim_parameters* rv = new sprockit::sim_parameters;
  std::set<std::string> key_names = map.getKeys();
  for(auto&& key : key_names) {
    rv->parse_keyval(
        key, map.find<std::string>(key), false, true, false);
  }
  rv->append_extra_data(&map);
  return rv;
}

namespace sstmac {

PyObject*
py_get_int_tuple(int num, const int* indices)
{
  PyObject* tuple = PyTuple_New(num);
  for (int i=0; i < num; ++i){
    PyObject* idx = PyInt_FromLong(indices[i]);
    PyTuple_SetItem(tuple, i, idx);
  }
  return tuple;
}

void
int_vector_from_py_array(PyObject* tuple, std::vector<int>& vec)
{
  Py_ssize_t size = PyTuple_Size(tuple);
  vec.resize(size);
  for (int i=0; i < size; ++i){
    PyObject* obj = PyTuple_GetItem(tuple,i);
    int item  = PyInt_AsLong(obj);
    vec[i] = item;
  }
}

void
py_extract_params(PyObject* dict, sprockit::sim_parameters* params)
{
#pragma GCC diagnostic ignored "-Wwrite-strings"
  PyObject* items = PyMapping_Items(dict);
  Py_ssize_t n_items = PySequence_Size(items);
  for(Py_ssize_t i = 0; i < n_items; ++i) {
    PyObject* pair = PySequence_GetItem(items, i);
    PyObject* key = PySequence_GetItem(pair, 0);
    PyObject* val = PySequence_GetItem(pair, 1);
    PyObject* key_str_obj = PyObject_Str(key);
    const char* key_c_str = PyString_AsString(key_str_obj);
    if (PyMapping_Check(val)){
      sprockit::sim_parameters* sub_params =
          params->get_optional_namespace(key_c_str);
      sstmac::py_extract_params(val, sub_params);
    } else {
      PyObject* val_str_obj = PyObject_Str(val);
      const char* val_c_str = PyString_AsString(val_str_obj);
      params->add_param_override(key_c_str, val_c_str);
      Py_DECREF(val_str_obj);
    }
    Py_DECREF(key_str_obj);
    Py_DECREF(val);
    Py_DECREF(key);
    Py_DECREF(pair);
  }
  Py_DECREF(items);
}

void
py_add_params(PyObject* dict, sprockit::sim_parameters* params)
{
  sprockit::sim_parameters::key_value_map::iterator it, end = params->end();
  for (it=params->begin(); it != end; ++it){
    const std::string& key_name = it->first;
    const std::string& key_value = it->second.value;
    PyObject* key = PyString_FromString(key_name.c_str());
    PyObject* val = PyString_FromString(key_value.c_str());
    PyDict_SetItem(dict, key, val);
  }
}

void
py_add_sub_params(PyObject* dict, sprockit::sim_parameters* params)
{
  sprockit::sim_parameters::namespace_iterator it, end = params->ns_end();
  for (it=params->ns_begin(); it != end; ++it){
    sprockit::sim_parameters* subparams = it->second;
    PyObject* subdict = PyDict_New();
    const char* key = it->first.c_str();
    PyDict_SetItemString(dict, key, subdict);
    sstmac::py_add_params(subdict, subparams);
    sstmac::py_add_sub_params(subdict, subparams);
  }
}

PyObject*
py_dict_from_params(sprockit::sim_parameters* params)
{
  PyObject* dict = PyDict_New();
  sstmac::py_add_params(dict, params);
  sstmac::py_add_sub_params(dict, params);
  return dict;
}

}

static PyObject*
set_debug_flags(PyObject* self, PyObject* args)
{
  Py_ssize_t size = PyTuple_Size(args);
  for (int i=0; i < size; ++i){
    PyObject* obj = PyTuple_GetItem(args,i);
    const char* str = PyString_AsString(obj);
    sprockit::debug::turn_on(str);
  }
  Py_RETURN_NONE;
}

static PyObject*
read_params(PyObject* self, PyObject* args)
{
  PyObject* sys_argv = PyTuple_GetItem(args, 0);
  int argc = PyList_Size(sys_argv);
  char** argv = new char*[argc];
  for (int i=0; i < argc; ++i){
    PyObject* obj = PyList_GetItem(sys_argv,i);
    const char* str = PyString_AsString(obj);
    argv[i] = const_cast<char*>(str);
  }

  sprockit::sim_parameters params;
  sstmac::try_main(&params, argc, argv, true/*only params*/);

  PyObject* dict = PyDict_New();
  sstmac::py_add_params(dict, &params);
  sstmac::py_add_sub_params(dict, &params);

  delete[] argv;

  return dict;
}

struct myMethod {
  const char* name;
  PyCFunction fxn;
};

static myMethod fxns[] = {
  { "readParams", read_params},
  { "debug", set_debug_flags},
};

static PyMethodDef sst_macro_integrated_methods[] = {
  { fxns[0].name, fxns[0].fxn, METH_VARARGS, "parse command line options and read parameters" },
  { fxns[1].name, fxns[1].fxn, METH_VARARGS, "set debug flags" },
  { NULL, NULL, 0, NULL }
};

#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef py3sstDef {
  PyModuleDef_HEAD_INIT,
  "sstmac", "", -1,
  sst_macro_integrated_methods
};
#endif

static void* gen_sst_macro_integrated_pymodule(void)
{
  static_assert( (sizeof(fxns)/sizeof(myMethod)) ==
      (((sizeof(sst_macro_integrated_methods))/sizeof(PyMethodDef)) - 1),
      "The size of the functions does not match");

#if PY_MAJOR_VERSION < 3
  PyObject* tmpModule = Py_InitModule("sstmac", sst_macro_integrated_methods);
#else
  PyObject* tmpModule = PyModule_Create(&py3sstDef);
#endif
  PyObject *code = Py_CompileString(py_sstmacro, "sstmacro", Py_file_input);
#pragma GCC diagnostic ignored "-Wwrite-strings"
  PyObject* module = PyImport_ExecCodeModule("sst.macro", code);


  int numMethods = sizeof(fxns) / sizeof(myMethod);
  for (int i=0; i < numMethods; ++i){
    PyObject* fxn = PyObject_GetAttrString(tmpModule, fxns[i].name);
    PyModule_AddObject(module, fxns[i].name, fxn);
  }

  /** Figure out nproc and nthread */
  PyObject* mainModule = PyImport_ImportModule("sst");
  PyObject* nproc_fxn = PyObject_GetAttrString(mainModule, "getMPIRankCount");
  PyObject* nthr_fxn = PyObject_GetAttrString(mainModule, "getThreadCount");

  PyObject* nthr_py = PyEval_CallObject(nthr_fxn, NULL);
  int nthread = PyInt_AsLong(nthr_py);
  Py_DECREF(nthr_py);
  Py_DECREF(nthr_fxn);
  PyObject* nproc_py = PyEval_CallObject(nproc_fxn, NULL);
  int nproc = PyInt_AsLong(nproc_py);
  Py_DECREF(nproc_py);
  Py_DECREF(nproc_fxn);
  Py_DECREF(mainModule);
  //for now, the topology will not distinguish nthread from nproc
  hw::topology::nproc = nproc*nthread;

  PyModule_AddIntConstant(module, "SwitchLogPInjectionPort", hw::logp_switch::Node);
  PyModule_AddIntConstant(module, "SwitchLogPNetworkPort", hw::logp_switch::Switch);
  PyModule_AddIntConstant(module, "NICMainInjectionPort", hw::nic::Injection);
  PyModule_AddIntConstant(module, "NICLogPInjectionPort", hw::nic::LogP);

  sstmac::py_init_system(module);
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  return module;
}

extern "C" {

ElementLibraryInfo macro_eli = {
    "macro",
    "SST Macroscale integrated components",
    NULL,                  // Components
    NULL,                              // Events
    NULL,                              // Introspectors
    NULL,                              // Modules
    NULL,
    NULL,                              // Partitioners
    gen_sst_macro_integrated_pymodule,  // Python Module Generator
    NULL
};


} // end extern "C"