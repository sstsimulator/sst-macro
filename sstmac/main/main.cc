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
#include <dharma/dharma/dharma_config.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <string>

#include <iostream>
#include <sstream>

#if !SSTMAC_INTEGRATED_CORE
int
main(int argc, char **argv)
{
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


