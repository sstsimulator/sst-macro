#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_cart.h>

namespace sstmac {
namespace sw {
/// Hello.
mpi_comm_cart::mpi_comm_cart(
  const mpi_comm_id &id, //const appid &aid,
  mpi_id rank, mpi_group* peers,
  app_manager*env, const app_id &aid, int ndims,
  int *dims, int *periods, int reorder) :
  mpi_comm(id, rank, peers, env, aid), ndims_(ndims), reorder_(reorder)
{

  for (int i = 0; i < ndims; i++) {
    dims_.push_back(dims[i]);
    periods_.push_back(periods[i]);
  }

  topotype_ = TOPO_CART;
}

void
mpi_comm_cart::set_coords(int rank, int* coords)
{
  int prev = 1;
  for (int i = 0; i < dims_.size(); i++) {
    int co = (rank / prev) % dims_[i];

    coords[i] = co;
    prev = prev * dims_[i];
  }
}

int
mpi_comm_cart::get_rank(int* coords)
{
  int rank = 0;
  int prev = 1;
  for (int i = 0; i < dims_.size(); i++) {
    rank += coords[i] * prev;
    prev *= dims_[i];
  }
  return rank;
}

int
mpi_comm_cart::shift(int dir, int dis)
{

  if (dir >= (int) dims_.size()) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpicomm_cart::shift: dir %d is too big for dims %d",
                     dir, dims_.size());
  }
  int coords[dims_.size()];
  set_coords(rank_, coords);
  coords[dir] += dis;

  if (coords[dir] >= dims_[dir]) {
    if (periods_[dir]) {
      coords[dir] = coords[dir] % dims_[dir];
      return get_rank(coords);
    }
    else {
      return mpi_comm::proc_null;
    }

  }
  else if (coords[dir] < 0) {
    if (periods_[dir]) {
      coords[dir] = (dims_[dir] + coords[dir]) % dims_[dir];
      return get_rank(coords);
    }
    else {
      return mpi_comm::proc_null;
    }
  }
  else {
    return get_rank(coords);
  }

}

}
}

