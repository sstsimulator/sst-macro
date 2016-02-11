
#include <Python.h>
#include <sst/core/element.h>

static const SST::ElementInfoComponent mpi_test_components[] = {
    {NULL, NULL, NULL, NULL}
};


static PyMethodDef mpi_test_methods[] = {
    { NULL, NULL, 0, NULL }
};

static void* gen_mpi_test_pymodule(void)
{
  PyObject* module = Py_InitModule("sst.sstmac_api_mpi_test", mpi_test_methods);
  return module;
}

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

