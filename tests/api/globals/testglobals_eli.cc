
#include <Python.h>
#include <sst/core/element.h>

static const SST::ElementInfoComponent global_test_components[] = {
    {NULL, NULL, NULL, NULL}
};


static PyMethodDef global_test_methods[] = {
    { NULL, NULL, 0, NULL }
};

static void* gen_global_test_pymodule(void)
{
  PyObject* module = Py_InitModule("sst.sstmac_api_globals_test", global_test_methods);
  return module;
}

extern "C" {

SST::ElementLibraryInfo sstmac_api_globals_test_eli = {
    "sstmac_api_globals_test",
    "SST Macroscale global variables testsuite",
    global_test_components,               // Components
    NULL,                              // Events
    NULL,                              // Introspectors
    NULL,                              // Modules
    NULL,                              // Partitioners
    NULL,                              // Generators
    gen_global_test_pymodule             // Python Module Generator

};

} // end extern "C"

