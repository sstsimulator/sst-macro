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

#if !SSTMAC_INTEGRATED_SST_CORE
int
main(int argc, char **argv)
{
#if CUSTOM_NEW_HANDLER
  std::set_new_handler(new_error_handler);
#endif

  int rc;

  try {
    sprockit::sim_parameters params;
    params.set_public_scope(false); //do not expose top-level params to subspaces
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