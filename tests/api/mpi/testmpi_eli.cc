
#include <Python.h>
#include <sst/core/element.h>

static const SST::ElementInfoComponent mpi_test_components[] = {
    {NULL, NULL, NULL, NULL}
};


static PyMethodDef mpi_test_methods[] = {
    { NULL, NULL, 0, NULL }
};

#if PY_MAJOR_VERSION < 3
static void* gen_mpi_test_pymodule(void)
{
  PyObject* module = Py_InitModule("sst.sstmac_api_mpi_test", mpi_test_methods);
  return module;
}
#else
static struct PyModuleDef py3sstDef {
  PyModuleDef_HEAD_INIT,
  "sst.sstmac_api_mpi_test", "", -1,
  mpi_test_methods
};

static void* gen_mpi_test_pymodule(void)
{
  return PyModule_Create(&py3sstDef);
}
#endif

extern "C" {

SST::ElementLibraryInfo sstmac_api_mpi_test_eli = {
    "sstmac_api_mpi_test",
    "SST Macroscale MPI testsuite",
    mpi_test_components,               // Components
    NULL,                              // Events
    NULL,                              // Introspectors
    NULL,                              // Modules
    NULL,                              // Partitioners
    NULL,                              // Generators
    gen_mpi_test_pymodule             // Python Module Generator

};

} // end extern "C"

