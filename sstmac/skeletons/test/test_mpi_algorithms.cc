#include <sprockit/test/test.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/common/logger.h>
#include <sstmac/common/runtime.h>

using sstmac::sstmac_runtime;

namespace pingall {

sstmac_register_app(mpi_algorithms);

int
mpi_algorithms_main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  MPI_Allgather(NULL, 100, MPI_INT, NULL, 100, MPI_INT, MPI_COMM_WORLD);

  MPI_Finalize();
  return 0;
}

}


