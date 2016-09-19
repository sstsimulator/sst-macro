
#include <sstmac/common/sstmac_config.h>
#include <sstmac/main/sstmac.h>

#include <sst/core/element.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/packet_flow/packet_flow_nic.h>
#include <sstmac/hardware/packet_flow/packet_flow_switch.h>
#include <sstmac/hardware/node/simple_node.h>

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


static sstmac::hw::topology* main_topology;

namespace sstmac {

PyObject*
py_get_int_tuple(int num, int* indices)
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

PyObject*
py_array_from_int_vector(const std::vector<int>& vec)
{
  PyObject* pVec = PyTuple_New(vec.size());
  for (int i = 0; i < vec.size(); ++i) {
       PyObject* pValue = PyInt_FromLong(vec[i]);
       if (!pValue) {
         spkt_abort_printf("cannot convert array value for building PyTuple");
       }
       PyTuple_SetItem(pVec, i, pValue);
  }
  return pVec;
}

void
py_extract_params(PyObject* dict, sprockit::sim_parameters* params)
{
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
  PyObject* flags = PyTuple_GetItem(args, 0);
  Py_ssize_t size = PyTuple_Size(flags);
  for (int i=0; i < size; ++i){
    PyObject* obj = PyTuple_GetItem(flags,i);
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

static const ElementInfoComponent macro_components[] = {
    sstmac::hw::packet_flow_switch_element_info,
    sstmac::hw::simple_node_element_info,
    {NULL, NULL, NULL, NULL}
};


static PyMethodDef sst_macro_integrated_methods[] = {
  { "readParams", read_params, METH_VARARGS, "parse command line options and read parameters" },
  { "debug", set_debug_flags, METH_VARARGS, "set debug flags" },
  { NULL, NULL, 0, NULL }
};

static void* gen_sst_macro_integrated_pymodule(void)
{
  PyObject* tmpModule = Py_InitModule("sstmac", sst_macro_integrated_methods);
  PyObject *code = Py_CompileString(py_sstmacro, "sstmacro", Py_file_input);
  PyObject* module = PyImport_ExecCodeModule("sst.macro", code);
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
    macro_components,                  // Components
    NULL,                              // Events
    NULL,                              // Introspectors
    NULL,                              // Modules
    NULL,                              // Partitioners
    NULL,                              // Generators
    gen_sst_macro_integrated_pymodule  // Python Module Generator
};


} // end extern "C"

