#include <sumi-mpi/mpi_api.h>

namespace sumi {

int
mpi_api::group_incl(MPI_Group oldgrp, int num_ranks, const int *ranks, MPI_Group *newgrp)
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
  mpi_group* newgrpPtr = new mpi_group(std::move(vec_ranks));
  add_group_ptr(newgrpPtr, newgrp);

  mpi_api_debug(sprockit::dbg::mpi, "MPI_Group_incl(%d,%d,*%d)",
                num_ranks, oldgrp, *newgrp);

  return MPI_SUCCESS;
}

void
mpi_api::group_create_with_id(MPI_Group group, int num_members, const uint64_t *members)
{
  int my_rank = comm_world()->rank();
  bool in_group = false;
  for (int i=0; i < num_members; ++i){
    if (members[i] == my_rank){
      in_group = true;
      break;
    }
  }

  if (!in_group) return; //I don't care

  std::vector<task_id> vec_ranks(num_members);
  for (int i=0; i < num_members; ++i){
    vec_ranks[i] = members[i];
  }
  mpi_group* grpPtr = new mpi_group(std::move(vec_ranks));
  add_group_ptr(grpPtr, &group);
}

int
mpi_api::group_free(MPI_Group *grp)
{
  auto iter = grp_map_.find(*grp);
  if (iter == grp_map_.end()){
    spkt_throw(sprockit::value_error,
               "Invalid MPI_Group %d passed to group free",
               *grp);
  }
  delete iter->second;
  grp_map_.erase(iter);
  *grp = MPI_GROUP_NULL;
  return MPI_SUCCESS;
}

}
