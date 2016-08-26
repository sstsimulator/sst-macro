#include <sumi-mpi/mpi_api.h>
#include <sstmac/software/process/operating_system.h>

namespace sumi {

int
mpi_api::group_incl(int *ranks, int num_ranks, MPI_Group oldgrp, MPI_Group *newgrp)
{
  mpi_group* oldgrpPtr = get_group(oldgrp);
  if (num_ranks > oldgrpPtr->size()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "MPI_Group_incl: invalid group size %d", num_ranks);
  }

  std::vector<task_id> vec_ranks(num_ranks, task_id(0));
  for (int i = 0; i < num_ranks; i++) {
    vec_ranks[i] = oldgrpPtr->at(ranks[i]);
  }
  mpi_group* newgrpPtr = new mpi_group(vec_ranks);
  *newgrp = add_group_ptr(newgrpPtr);

  mpi_api_debug(sprockit::dbg::mpi, "MPI_Group_incl(%d,%d,*%d)",
                num_ranks, oldgrp, *newgrp);

  return MPI_SUCCESS;
}

}
