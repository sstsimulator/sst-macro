
#ifndef SSTMAC_MICRO_INTEGRATED_SST_CORE_H_
#define SSTMAC_MICRO_INTEGRATED_SST_CORE_H_

#include <sst_config.h>

#include <sst/core/element.h>
#include <sst/core/params.h>

#include <sprockit/unordered.h>
#include <sprockit/sim_parameters.h>

#include <sstmac/hardware/common/connection.h>

#include <Python.h>

namespace sstmac {

void py_init_system(PyObject*  module);

void py_extract_params(PyObject* dict, sprockit::sim_parameters* params);

PyObject* py_get_int_tuple(int num, int* indices);

void int_vector_from_py_array(PyObject* tuple, std::vector<int>& vec);

PyObject* py_array_from_int_vector(const std::vector<int>& vec);

void py_add_params(PyObject* dict, sprockit::sim_parameters* params);

void py_add_sub_params(PyObject* dict, sprockit::sim_parameters* params);

PyObject* py_dict_from_params(sprockit::sim_parameters* params);

} // end namespace sstmac

#endif /* SSTMAC_MICRO_INTEGRATED_SST_CORE_H_ */

