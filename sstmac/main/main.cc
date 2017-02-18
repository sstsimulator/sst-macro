#include <sstmac/main/sstmac.h>
#include <sstmac/common/sstmac_config.h>
#include <iostream>
#include <sstream>
#include <iterator>
#include <stdint.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <sprockit/malloc.h>
#include <sprockit/spkt_new.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/sprockit/spkt_config.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sumi/sumi/sumi_config.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <string>

#include <iostream>
#include <sstream>

#if CUSTOM_NEW_HANDLER
#include <execinfo.h>
#include <dlfcn.h>
#include <signal.h>
#include <cxxabi.h>

void new_error_handler()
{
  std::cerr << "memory allocation error" << std::endl;
  void* array[40];
  char cmd[1024];
  char debug[1024];
  int size = backtrace(array, 40);
  char** symbols = backtrace_symbols(array, size);
  for (int i=0; i < size; ++i){
    void* addr = array[i];
    Dl_info info;
    int err = dladdr(addr, &info);
    if (err && info.dli_sname){
      char* demangled = NULL;
      int status = -1;
      if (info.dli_sname[0] == '_')
        demangled = abi::__cxa_demangle(info.dli_sname, NULL, 0, &status);

      fprintf(stderr, "%-3d %*p %s + %zd : %s %p\n",
        i, int(2 + sizeof(void*)), array[i],
        status == 0 ? demangled : info.dli_sname == 0 ? symbols[i] : info.dli_sname,
        (char*) array[i] - (char*)info.dli_saddr,
        info.dli_fname ? info.dli_fname : "no file",
        info.dli_fbase ? info.dli_fbase : "");

      //fprintf(stderr, "backtrace[%2d] = %p : %s %p\n",
      // i, addr, info.dli_fname, info.dli_fbase);
      if (demangled) free(demangled);
    } else {
      fprintf(stderr, "no symbol - %d %p : %s\n",
        err, info.dli_sname,
        info.dli_fname ? info.dli_fname : "no file",
        info.dli_fbase ? info.dli_fbase : "");
    }
  }
  std::set_new_handler(nullptr);
}
#endif

#if !SSTMAC_INTEGRATED_CORE
int
main(int argc, char **argv)
{
#if CUSTOM_NEW_HANDLER
  std::set_new_handler(new_error_handler);
#endif

  int rc;

  try {
    sprockit::sim_parameters params;
    bool params_only = false;
    sstmac::try_main(&params, argc, argv, params_only);
  }
  catch (const std::exception &e) {
    std::cout.flush();
    std::cerr.flush();
    std::cerr << argv[0] << ": caught exception while setting up simulation:\n"
              << e.what() << "\n";
    return 1;
  }
  catch (...) {
    std::cerr << argv[0]
              << ": caught unknown exception while setting up simulation\n";
    return 1;
  }

  return 0;
}
#endif // !SSTMAC_INTEGRATED_SST_CORE


