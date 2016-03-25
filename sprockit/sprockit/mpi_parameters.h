#ifndef sprockit_MPI_PARAMETERS_H
#define sprockit_MPI_PARAMETERS_H

#include <sprockit/sim_parameters.h>
#include <mpi.h>

namespace sprockit {

class mpi_param_bcaster:
  public param_bcaster
{
 public:
   void
   bcast(void *buf, int size, int me, int root){
     MPI_Bcast(buf, size, MPI_BYTE, root, MPI_COMM_WORLD);
   }
};

static inline sim_parameters*
MPI_Bcast_params(const std::string& fname)
{
  int rank; MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  int nproc; MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  mpi_param_bcaster bc;
  sprockit::sim_parameters* params = new sprockit::sim_parameters;
  sim_parameters::parallel_build_params(params, rank, nproc, fname, &bc);
  return params;
}

}

#endif // MPI_PARAMETERS_H
