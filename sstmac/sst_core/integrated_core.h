
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

/**
 * @brief py_init_system Initializes a class type "System" into the module that can
 *        be used for setting up SST/macro simulations via python
 * @param module  The module to add the Python class "System" to
 */
void py_init_system(PyObject* module);

/**
 * @brief py_extract_params Read all the keyword params in a Python dictionary
 *        and store them into a sprockit params object. This recursively
 *        follows the dictionary to create namespaces
 * @param dict    [in]
 * @param params  [out]
 */
void py_extract_params(PyObject* dict, sprockit::sim_parameters* params);

/**
 * @brief py_get_int_tuple  Convert an integer array into a python tuple oject
 * @param num
 * @param indices
 * @return A python tuple with the indices
 */
PyObject* py_get_int_tuple(int num, const int* indices);

/**
 * @brief int_vector_from_py_array Read all the values in a Python tuple (should all be integers)
 *        and store them in a std vector
 * @param tuple Input tuple
 * @param vec Will be cleared and resized
 */
void int_vector_from_py_array(PyObject* tuple, std::vector<int>& vec);

/**
 * @brief py_array_from_int_vector
 * @param vec
 * @return
 */
inline PyObject* py_tuple_from_int_vector(const std::vector<int>& vec){
  return py_get_int_tuple(vec.size(), vec.data());
}

/**
 * @brief py_add_params Take all the key-value parameters in a sprockit params object
 *                      and store them in a Python dictionary. This does NOT go
 *                      recursively through namespaces
 * @param dict    [out]
 * @param params  [in]
 */
void py_add_params(PyObject* dict, sprockit::sim_parameters* params);

/**
 * @brief py_add_sub_params Take all the key-value parameters in a sprockit params object
 *                      and store them in a Python dictionary. This does go
 *                      recursively through namespaces
 * @param dict    [out]
 * @param params  [in]
 */
void py_add_sub_params(PyObject* dict, sprockit::sim_parameters* params);

/**
 * @brief py_dict_from_params Wrapper to py_add_params that creates a new Python dictionary
 *        and stores all the key-value pairs in params
 * @param params
 * @return A python dictionary (recursive for namespaces)
 */
PyObject* py_dict_from_params(sprockit::sim_parameters* params);

} // end namespace sstmac

#endif /* SSTMAC_MICRO_INTEGRATED_SST_CORE_H_ */

