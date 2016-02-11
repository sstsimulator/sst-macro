#include <sstmac/software/process/user_app.h>
#include <sstmac/software/process/backtrace.h>

namespace sstmac {
namespace sw {

main_fxn user_skeleton_main = NULL;

void
user_app::skeleton_main()
{
  SSTMACBacktrace("main");
  if (user_skeleton_main == NULL){
    spkt_throw(sprockit::null_error,
      "user_app::skeleton_main: main function not initialized");
  }
  (*user_skeleton_main)(argc(), argv());
}

void
user_app::init_factory_params(sprockit::sim_parameters* params)
{
  /**
      sstobject {
          name = MPI;
          gui = yes;
          docstring = The MPI API used by the application;
          type = sstmac::sw::mpiapi;
          yesno = true;
      }
  */

  /**
      sstobject {
          name = SHMEM;
          gui = no;
          docstring = The SHMEM API used by the application;
          type = sstmac::sw::shmem::api;
          yesno = true;
      }
  */
  app::init_factory_params(params);
}

}
}



