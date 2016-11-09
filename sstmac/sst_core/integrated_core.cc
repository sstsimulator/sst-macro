
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

DeclareDebugSlot(timestamp);
static bool checked_prefix_fxn = false;

static sprockit::sim_parameters*
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

class timestamp_prefix_fxn :
  public sprockit::debug_prefix_fxn
{
 public:
  timestamp_prefix_fxn(sstmac::event_scheduler* mgr) : mgr_(mgr){}

  std::string
  str() {
    double t_ms = mgr_->now().msec();
    return sprockit::printf("T=%12.8e ms:", t_ms);
  }

 private:
  event_scheduler* mgr_;
};

template <class T>
SST::Component*
create_component(SST::ComponentId_t id, SST::Params& params){
  sprockit::sim_parameters* macParams =
    make_spkt_params_from_sst_params(params);

  T* created = new T(macParams, id, nullptr);
  if (!checked_prefix_fxn){
    if (sprockit::debug::slot_active(sprockit::dbg::timestamp)){
      sprockit::debug_prefix_fxn* fxn = new timestamp_prefix_fxn(created);
      sprockit::debug::prefix_fxn = fxn;
    }
    checked_prefix_fxn = true;
  }

  created->init_links(macParams);
  return created;
}

template <class T>
SST::SubComponent*
create_subcomponent(SST::Component* parent, SST::Params& params){
  sprockit::sim_parameters* macParams =
    make_spkt_params_from_sst_params(params);
  T* created = new T(macParams, parent);
  created->init_links(macParams);
  return created;
}

static const ElementInfoPort ports[] = {
 {"input %(out)d %(in)d",  "Will receive new payloads here",      NULL},
 {"output %(out)d %(in)d", "Will receive new acks(credits) here", NULL},
 {"in-out %(out)d %(in)d", "Will send/recv payloads here",       NULL},
 {"rtr", "Special link to Merlin router", NULL},
 {NULL, NULL, NULL}
};

static const ElementInfoSubComponent subcomponents[] = {
  { "pisces",
    "Link Control module for building Pisces NICs",
    NULL,
    create_subcomponent<hw::pisces_simple_network>,
    NULL,
    NULL,
    "SST::Interfaces::SimpleNetwork"
  },
  { NULL, NULL, NULL, NULL, NULL, NULL }
};

const static SST::ElementInfoComponent pisces_switch_element_info = {
  "pisces_switch",
  "A network switch implementing a packet-flow congestion model",
  NULL,
  create_component<hw::pisces_switch>,
  NULL,
  ports,
  COMPONENT_CATEGORY_NETWORK
};

const static SST::ElementInfoComponent logp_switch_element_info = {
  "logp_switch",
  "A network switch implementing a LogP congestion model",
  NULL,
  create_component<hw::logp_switch>,
  NULL,
  ports,
  COMPONENT_CATEGORY_NETWORK
};

const static SST::ElementInfoComponent simple_node_element_info = {
  "simple_node",
  "A node with basic OS and basic compute functionality",
  NULL,
  create_component<hw::simple_node>,
  NULL,
  ports,
  COMPONENT_CATEGORY_PROCESSOR
};

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

static const ElementInfoComponent macro_components[] = {
    pisces_switch_element_info,
    simple_node_element_info,
    logp_switch_element_info,
    {NULL, NULL, NULL, NULL}
};

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

static void* gen_sst_macro_integrated_pymodule(void)
{
  static_assert( (sizeof(fxns)/sizeof(myMethod)) ==
      (((sizeof(sst_macro_integrated_methods))/sizeof(PyMethodDef)) - 1),
      "The size of the functions does not match");

  PyObject* tmpModule = Py_InitModule("sstmac", sst_macro_integrated_methods);
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
    macro_components,                  // Components
    NULL,                              // Events
    NULL,                              // Introspectors
    NULL,                              // Modules
    subcomponents,
    NULL,                              // Partitioners
    gen_sst_macro_integrated_pymodule,  // Python Module Generator
    NULL
};


} // end extern "C"

