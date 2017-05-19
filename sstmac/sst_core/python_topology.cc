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

#include <Python.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sprockit/unordered.h>

extern "C" {

typedef struct {
    PyObject_HEAD
    sstmac::hw::topology* macro_topology;
    sprockit::sim_parameters* params;
} SystemPy_t;

} // end extern "C"

static PyObject*
sys_num_switches(SystemPy_t* self, PyObject* args);

static PyObject*
sys_num_nodes(SystemPy_t* self, PyObject* args);

static int
sys_init(SystemPy_t* self, PyObject* args, PyObject* kwargs);

static void
sys_dealloc(SystemPy_t* self);

static PyObject*
sys_get_injection_connections(SystemPy_t* self, PyObject* idx);

static PyObject*
sys_get_switch_params(SystemPy_t* self, PyObject* idx);

static PyObject*
sys_get_switch_connections(SystemPy_t* self, PyObject* idx);

static PyObject*
sys_get_ejection_connections(SystemPy_t* self, PyObject* idx);

static PyObject*
sys_node_to_logp_switch(SystemPy_t* self, PyObject* idx);

static PyObject*
sys_is_logp(SystemPy_t* self, PyObject* null);

static int
sys_init(SystemPy_t* self, PyObject* args, PyObject* kwargs);

static PyMethodDef system_methods[] = {
  { "nodeToLogPSwitch",
    (PyCFunction)sys_node_to_logp_switch, METH_O,
      "map a node id to its corresponding LogP switch" },
  { "isLogP",
    (PyCFunction)sys_is_logp, METH_NOARGS,
      "return whether to do simple LogP build" },
  { "injectionConnections",
    (PyCFunction)sys_get_injection_connections, METH_O,
      "get the switch id and ports for injecting to a given node" },
  { "ejectionConnections",
    (PyCFunction)sys_get_ejection_connections, METH_O,
    "get the switch id and ports for injecting to a given node" },
  { "switchConnections",
    (PyCFunction)sys_get_switch_connections, METH_O,
      "get the switches and associated ports for a sw-sw connection"},
  { "switchParams",
    (PyCFunction)sys_get_switch_params, METH_O,
    "get params unique to a given switch" },
  { "numNodes",
    (PyCFunction)sys_num_nodes, METH_NOARGS,
    "return the number of nodes in the topology"
  },
  { "numSwitches",
    (PyCFunction)sys_num_switches, METH_NOARGS,
    "return the number of switches in the topology"
  },
  { NULL, NULL, 0, NULL }
};



static PyTypeObject SystemType = {
#if PY_MAJOR_VERSION >= 3
    PyVarObject_HEAD_INIT(NULL, 0)
#else
    PyObject_HEAD_INIT(NULL)
    0,                         /* ob_size */
#endif
    "sst.macro.Topology",      /* tp_name */
    sizeof(SystemPy_t),        /* tp_basicsize */
    0,                         /* tp_itemsize */
    (destructor)sys_dealloc,   /* tp_dealloc */
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
    "SST/macro system",        /* tp_doc */
    0,                         /* tp_traverse */
    0,                         /* tp_clear */
    0,                         /* tp_richcompare */
    0,                         /* tp_weaklistoffset */
    0,                         /* tp_iter */
    0,                         /* tp_iternext */
    system_methods,               /* tp_methods */
    0,                         /* tp_members */
    0,                         /* tp_getset */
    0,                         /* tp_base */
    0,                         /* tp_dict */
    0,                         /* tp_descr_get */
    0,                         /* tp_descr_set */
    0,                         /* tp_dictoffset */
    (initproc)sys_init,        /* tp_init */
    0,                         /* tp_alloc */
    0,                         /* tp_new */
};

namespace sstmac {

void py_init_system(PyObject*  module)
{
  // Initialize our types
  SystemType.tp_new = PyType_GenericNew;
  if ( ( PyType_Ready(&SystemType) ) ) {
      abort();
      // TODO Figure out the right way to raise an error here
      // output->fatal(CALL_INFO, -1, "Error loading Python types.\n");
  }

#if ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif
    Py_INCREF(&SystemType);
#if ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))
#pragma GCC diagnostic pop
#endif
  PyModule_AddObject(module, "System", (PyObject*)&SystemType);
}

}

static PyObject*
sys_get_injection_connections(SystemPy_t* self, PyObject* nodeIdx)
{
  int nid = PyInt_AsLong(nodeIdx);
  int ports[32];
  int num_ports;
  sstmac::switch_id sid = self->macro_topology
      ->netlink_to_injection_switch(nid, ports, num_ports);
  PyObject* intTuple = sstmac::py_get_int_tuple(num_ports, ports);
  PyObject* tuple = PyTuple_New(2);
  PyObject* swIdx = PyInt_FromLong(sid);
  PyTuple_SetItem(tuple, 0, swIdx);
  PyTuple_SetItem(tuple, 1, intTuple);
  return tuple;
}

static PyObject*
sys_get_switch_params(SystemPy_t* self, PyObject* idx)
{
  int swIdx = PyInt_AsLong(idx);
  sprockit::sim_parameters* switch_params
      = self->params->get_namespace("switch");
  if (!self->macro_topology->uniform_switches()){
    self->macro_topology->configure_nonuniform_switch_params(swIdx, switch_params);
  }
  return sstmac::py_dict_from_params(switch_params);
}

static PyObject*
sys_get_switch_connections(SystemPy_t* self, PyObject* idx)
{
  int swIdx = PyInt_AsLong(idx);
  std::vector<sstmac::hw::topology::connection> conns;
  self->macro_topology->connected_outports(swIdx, conns);
  PyObject* tuple = PyTuple_New(conns.size());
  for (int i=0; i < conns.size(); ++i){
    sstmac::hw::topology::connection& conn = conns[i];
    PyObject* connTuple = PyTuple_New(4);
    PyObject* srcIdx = PyInt_FromLong(conn.src);
    PyTuple_SetItem(connTuple, 0, srcIdx);
    PyObject* dstIdx = PyInt_FromLong(conn.dst);
    PyTuple_SetItem(connTuple, 1, dstIdx);
    PyObject* srcOutport = PyInt_FromLong(conn.src_outport);
    PyTuple_SetItem(connTuple, 2, srcOutport);
    PyObject* dstInport = PyInt_FromLong(conn.dst_inport);
    PyTuple_SetItem(connTuple, 3, dstInport);
    PyTuple_SetItem(tuple, i, connTuple);
  }
  return tuple;
}

static PyObject*
sys_node_to_logp_switch(SystemPy_t* self, PyObject* idx)
{
  int nid = PyInt_AsLong(idx);
  int sid = self->macro_topology->node_to_logp_switch(nid);
  return PyInt_FromLong(sid);
}

static PyObject*
sys_get_ejection_connections(SystemPy_t* self, PyObject* nodeIdx)
{
  int nid = PyInt_AsLong(nodeIdx);
  int ports[32];
  int num_ports;
  sstmac::switch_id sid = self->macro_topology
      ->netlink_to_ejection_switch(nid, ports, num_ports);
  PyObject* intTuple = sstmac::py_get_int_tuple(num_ports, ports);
  PyObject* tuple = PyTuple_New(2);
  PyObject* swIdx = PyInt_FromLong(sid);
  PyTuple_SetItem(tuple, 0, swIdx);
  PyTuple_SetItem(tuple, 1, intTuple);
  return tuple;
}

static bool is_logp(SystemPy_t* self)
{
  sprockit::sim_parameters* switch_params = self->params->get_namespace("switch");
  std::string model = switch_params->get_param("model");
  return model == "logP" || model == "LogP";
}

static PyObject*
sys_is_logp(SystemPy_t *self, PyObject *null)
{
  bool test = is_logp(self);
  PyObject* theBool = PyBool_FromLong(test);
  return theBool;
}

static int
sys_init(SystemPy_t* self, PyObject* args, PyObject* kwargs)
{
  self->params = new sprockit::sim_parameters;

  PyObject* params_dict = NULL;
  PyArg_ParseTuple(args, "|O", &params_dict);
  if(params_dict != NULL) {
    if(PyMapping_Check(params_dict) != 1) {
      // TODO figure out how to correctly raise an exception here @integrated_core
      std::cerr << "Positional argument to Topology constructor, if given, must be a mapping" << std::endl;
      return -1;
    }
  }
  sstmac::py_extract_params(params_dict, self->params);

  if (PyMapping_Check(kwargs)){
    sstmac::py_extract_params(kwargs, self->params);
  }

  self->macro_topology = sstmac::hw::topology::static_topology(self->params);

  sprockit::sim_parameters* switch_params = self->params->get_namespace("switch");
  if (is_logp(self)){
    //I guess we don't do anything?
  } else {
    self->macro_topology->configure_individual_port_params(sstmac::switch_id(0), switch_params);
  }

  return 0;
}

#ifndef Py_TYPE
 #define Py_TYPE(ob) (((PyObject*)(ob))->ob_type)
#endif

static void
sys_dealloc(SystemPy_t* self)
{
  if(self->params) delete self->params;
  Py_TYPE(self)->tp_free((PyObject*)self);
}

static PyObject*
sys_num_switches(SystemPy_t* self, PyObject* null)
{
  return PyInt_FromLong(self->macro_topology->num_switches());
}

static PyObject*
sys_num_nodes(SystemPy_t* self, PyObject* null)
{
  return PyInt_FromLong(self->macro_topology->num_nodes());
}