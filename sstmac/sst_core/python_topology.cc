#include <Python.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sprockit/unordered.h>

using namespace sstmac;

extern "C" {

typedef struct {
    PyObject_HEAD
    sstmac::hw::topology* macro_topology;
    spkt_unordered_map<std::string, std::string>* params;
} TopologyPy_t;

} // end extern "C"

static PyObject*
topo_num_switches(TopologyPy_t* self, PyObject* args);

static PyObject*
topo_num_nodes(TopologyPy_t* self, PyObject* args);

static PyObject*
topo_register_switch(TopologyPy_t* self, PyObject* args);

static PyObject*
topo_register_endpoint(TopologyPy_t* self, PyObject* args);

static int topoInit(TopologyPy_t* self, PyObject* args, PyObject* kwargs);
static void topoDealloc(TopologyPy_t* self);
static PyObject* topo_connect_switches(TopologyPy_t* self, PyObject* args, PyObject* kwargs);
static PyObject* topo_connect_endpoints(TopologyPy_t* self, PyObject* args, PyObject* kwargs);
static PyObject* topo_connect_node_to_nic(TopologyPy_t* self, PyObject* args, PyObject* kwargs);

static PyMethodDef topologyMethods[] = {
    { "register_switch",
      (PyCFunction)topo_register_switch, METH_VARARGS,
      "register a switch to be included in the SST/macro topology"
    },
    { "register_endpoint",
      (PyCFunction)topo_register_endpoint, METH_VARARGS,
      "register an endpoint to be included in the SST/macro topology"
    },
    { "connect_switches",
      (PyCFunction)topo_connect_switches, METH_VARARGS | METH_KEYWORDS,
      "connect the registered switches to form this topology"
    },
    { "connect_endpoints",
      (PyCFunction)topo_connect_endpoints, METH_VARARGS | METH_KEYWORDS,
      "connect the registered endpoints to the correct switches necessary form this topology"
    },
    { "connect_node_to_nic",
      (PyCFunction)topo_connect_node_to_nic, METH_VARARGS | METH_KEYWORDS,
      "connect a node to its corresponding nic"
    },
    { "num_nodes",
      (PyCFunction)topo_num_nodes, METH_VARARGS,
      "return the number of nodes in the topology"
    },
    { "num_switches",
      (PyCFunction)topo_num_switches, METH_VARARGS,
      "return the number of switches in the topology"
    },
    { NULL, NULL, 0, NULL }
};



static PyTypeObject TopologyType = {
    PyObject_HEAD_INIT(NULL)
    0,                         /* ob_size */
    "sst.macro.Topology",      /* tp_name */
    sizeof(TopologyPy_t),      /* tp_basicsize */
    0,                         /* tp_itemsize */
    (destructor)topoDealloc,   /* tp_dealloc */
    0,                         /* tp_print */
    0,                         /* tp_getattr */
    0,                         /* tp_setattr */
    0,                         /* tp_compare */
    0,                         /* tp_repr */
    0,                         /* tp_as_number */
    0,                         /* tp_as_sequence */
    0,                         /* tp_as_mapping */
    0,                         /* tp_hash */
    0,                         /* tp_call */
    0,                         /* tp_str */
    0,                         /* tp_getattro */
    0,                         /* tp_setattro */
    0,                         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,        /* tp_flags */
    "SST/macro topology",      /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    topologyMethods,           /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    (initproc)topoInit,        /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

void
init_python_topology(PyObject*  module)
{
  // Initialize our types
  TopologyType.tp_new = PyType_GenericNew;
  if ( ( PyType_Ready(&TopologyType) ) ) {
      abort();
      // TODO Figure out the right way to raise an error here
      // output->fatal(CALL_INFO, -1, "Error loading Python types.\n");
  }

#if ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif
    Py_INCREF(&TopologyType);
#if ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
#pragma GCC diagnostic pop
#endif
  PyModule_AddObject(module, "Topology", (PyObject*)&TopologyType);
}


static int
topoInit(TopologyPy_t* self, PyObject* args, PyObject* kwargs)
{
  std::vector<PyObject*> items_lists_to_check;


  spkt_unordered_map<std::string, std::string>* params_map =
      new spkt_unordered_map<std::string, std::string>();

  PyObject* params_dict = NULL;
  PyArg_ParseTuple(args, "|O", &params_dict);
  if(params_dict != NULL) {
    if(PyMapping_Check(params_dict) != 1) {
      // TODO figure out how to correctly raise an exception here @integrated_core
      std::cerr << "Positional argument to Topology constructor, if given, must be a mapping" << std::endl;
      return -1;
    }
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
    PyObject* items = PyMapping_Items(params_dict);
#pragma GCC diagnostic pop
    items_lists_to_check.push_back(items);
    // decref will happen later
  }

  if (PyMapping_Check(kwargs)){
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
    PyObject* kwargs_items = PyMapping_Items(kwargs);
#pragma GCC diagnostic pop
    items_lists_to_check.push_back(kwargs_items);
    // decref will happen later
  }

  for(int ilist = 0; ilist < items_lists_to_check.size(); ++ilist) {
    PyObject* items = items_lists_to_check[ilist];

    Py_ssize_t n_items = PySequence_Size(items);
    for(Py_ssize_t i = 0; i < n_items; ++i) {
      PyObject* pair = PySequence_GetItem(items, i);
      PyObject* key = PySequence_GetItem(pair, 0);
      PyObject* val = PySequence_GetItem(pair, 1);

      PyObject* key_str_obj = PyObject_Str(key);
      PyObject* val_str_obj = PyObject_Str(val);

      const char* key_c_str = PyString_AsString(key_str_obj);
      const char* val_c_str = PyString_AsString(val_str_obj);

      //append topology namespace to all params
      std::string ns_key_str = sprockit::printf("topology.%s", key_c_str);
      (*params_map)[ns_key_str] = val_c_str;

      Py_DECREF(val_str_obj);
      Py_DECREF(key_str_obj);
      Py_DECREF(val);
      Py_DECREF(key);
      Py_DECREF(pair);
    }

    Py_DECREF(items);

  }

  self->params = params_map;

  sprockit::sim_parameters* sim_params = make_sim_params_from_mapping(*self->params);

  self->macro_topology = hw::topology::static_topology(sim_params);

  delete sim_params;

  return 0;

}

static void
topoDealloc(TopologyPy_t* self)
{
  if(self->params) delete self->params;
  self->ob_type->tp_free((PyObject*)self);
}

namespace _impl {

template <typename MapT, typename RegIdT>
static void
_topo_register(
    TopologyPy_t* self, PyObject* args,
    connectable_proxy_component::component_category_t cmp_cat,
    MapT& reg_comps, RegIdT& id
)
{
  PyObject* py_comp;
  PyArg_ParseTuple(args, "O", &py_comp);

  connectable_proxy_component* this_comp = new connectable_proxy_component;

  // Make sure it doesn't get deleted, now that we're storing it
  Py_INCREF(py_comp);
  this_comp->component_proxy = py_comp;

  // Add the topology parameters to the SST::Params
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
  for(auto&& pair : *self->params) {
    std::string top_key = "topology." + pair.first;
    PyObject* discarded = PyObject_CallMethod(py_comp, "addParam", "ss",
        top_key.c_str(), pair.second.c_str()
    );
    if(discarded) Py_DECREF(discarded);
  }

  {
    PyObject* full_name = PyObject_CallMethod(py_comp, "getFullName", "");
    const char* comp_name = PyString_AsString(full_name);
    this_comp->component_name = std::string(comp_name);
    if(full_name) Py_DECREF(full_name);
  } // let comp_name go out of scope so we don't accidentally use it later
#pragma GCC diagnostic pop

  this_comp->category = cmp_cat;

  reg_comps[id] = this_comp;

}

} // end namespace _impl

static PyObject*
topo_num_switches(TopologyPy_t* self, PyObject* args)
{
  return PyInt_FromLong(self->macro_topology->num_switches());
}

static PyObject*
topo_num_nodes(TopologyPy_t* self, PyObject* args)
{
  return PyInt_FromLong(self->macro_topology->num_nodes());
}

static PyObject*
topo_register_switch(
    TopologyPy_t* self, PyObject* args
)
{
  static int current_switch_id = 0;
  static connectable_proxy_component::switch_map& reg_switches =
      connectable_proxy_component::registered_switches;
  switch_id id(current_switch_id++);

  _impl::_topo_register(
      self, args, connectable_proxy_component::Switch,
      reg_switches, id
  );

  Py_RETURN_NONE;
}

static PyObject*
topo_register_endpoint(
    TopologyPy_t* self, PyObject* args
)
{
  static int current_endpoint_id = 0;
  static connectable_proxy_component::endpoint_map& reg_endpoints =
      connectable_proxy_component::registered_endpoints;
  endpoint_id id(current_endpoint_id++);

  _impl::_topo_register(
      self, args, connectable_proxy_component::Endpoint,
      reg_endpoints, id
  );

  Py_RETURN_NONE;
}

static PyObject*
topo_connect_switches(
    TopologyPy_t* self, PyObject* args, PyObject* kwargs
)
{
  static connectable_proxy_component::switch_map& reg_switches =
      connectable_proxy_component::registered_switches;

  char* hop_latency;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
  char* keywords[] = { "hop_latency", NULL };
  PyArg_ParseTupleAndKeywords(args, kwargs, "s", keywords,
      &hop_latency
  );
#pragma GCC diagnostic pop

  for(auto& pair : reg_switches) {
    pair.second->hop_latency = hop_latency;
  }

  self->macro_topology->connect_topology(reg_switches);

  Py_RETURN_NONE;

}

static PyObject*
topo_connect_endpoints(
    TopologyPy_t* self, PyObject* args, PyObject* kwargs
)
{
  static connectable_proxy_component::switch_map& reg_switches =
      connectable_proxy_component::registered_switches;
  static connectable_proxy_component::endpoint_map& reg_endpoints =
      connectable_proxy_component::registered_endpoints;

  char* injection_latency;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
  char* keywords[] = { "injection_latency", NULL };
  PyArg_ParseTupleAndKeywords(args, kwargs, "s", keywords,
      &injection_latency
  );
#pragma GCC diagnostic pop

  for(auto& pair : reg_endpoints) {
    pair.second->injection_latency = injection_latency;
  }
  for(auto& pair : reg_switches) {
    pair.second->injection_latency = injection_latency;
  }

  self->macro_topology->connect_end_points(reg_switches, reg_endpoints);

  Py_RETURN_NONE;
}

static PyObject*
topo_connect_node_to_nic(
    TopologyPy_t* self, PyObject* args, PyObject* kwargs
)
{
  PyObject* node;
  PyObject* nic;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
  char* keywords[] = { "node", "nic", NULL };
  PyArg_ParseTupleAndKeywords(args, kwargs, "OO", keywords,
      &node, &nic
  );

  PyObject* node_name_obj = PyObject_CallMethod(node, "getFullName", "");
  const char* node_name = PyString_AsString(node_name_obj);
  PyObject* nic_name_obj = PyObject_CallMethod(nic, "getFullName", "");
  const char* nic_name = PyString_AsString(nic_name_obj);
#pragma GCC diagnostic pop


  std::string link_name = sprockit::printf(
      "__auto_link_node_(%s)_to_nic_(%s)",
      node_name, nic_name
  );
  std::string port_name_prefix = sprockit::printf(
      "__auto_port_nodetonic_(%s)_to_(%s)",
      node_name, nic_name
  );
  std::string node_port_name = sprockit::printf("%s_node", port_name_prefix.c_str());
  std::string nic_port_name = sprockit::printf("%s_nic", port_name_prefix.c_str());

  // We DO actually want a 0 latency link between the node and the nic
  std::string latency_str = "0 ms";

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"
  PyObject* link = PyObject_CallMethod(
      connectable_proxy_component::sst, "Link", "s", link_name.c_str()
  );
  PyObject* __discarded_result = PyObject_CallMethod(link, "connect", "(Oss)(Oss)",
      node, node_port_name.c_str(), latency_str.c_str(),
      nic, nic_port_name.c_str(), latency_str.c_str()
  );
#pragma GCC diagnostic pop

  Py_DECREF(__discarded_result);

  Py_DECREF(link);
  Py_DECREF(node_name_obj);
  Py_DECREF(nic_name_obj);

  Py_RETURN_NONE;
}
