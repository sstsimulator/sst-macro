#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_comm/mpi_comm_cart.h>
#include <sstmac/software/process/backtrace.h>
#include <sprockit/stl_string.h>
#include <sstmac/software/process/operating_system.h>

namespace sumi {

int
mpi_api::comm_dup(MPI_Comm input, MPI_Comm *output)
{
  check_init();
  start_mpi_call("MPI_Comm_dup");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_dup(%s) start",
                comm_str(input).c_str()); 
  mpi_comm* inputPtr = get_comm(input);
  mpi_comm* outputPtr = comm_factory_->comm_dup(inputPtr);
  *output = add_comm_ptr(outputPtr);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_dup(%s,*%s) finish",
                comm_str(input).c_str(), comm_str(*output).c_str());
  return MPI_SUCCESS;
}

int
mpi_api::comm_size(MPI_Comm comm, int *size)
{
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Comm_size(%s)", comm_str(comm).c_str());
  *size = get_comm(comm)->size();
  return MPI_SUCCESS;
}

int
mpi_api::comm_create(MPI_Comm input, MPI_Group group, MPI_Comm *output)
{
  start_mpi_call("MPI_Comm_create");
  mpi_comm* inputPtr = get_comm(input);
  mpi_group* groupPtr = get_group(group);
  *output = add_comm_ptr(comm_factory_->comm_create(inputPtr, groupPtr));
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_create(%s,%d,*%s)",
                comm_str(input).c_str(), group, comm_str(*output).c_str());
  return MPI_SUCCESS;
}

int
mpi_api::comm_group(MPI_Comm comm, MPI_Group* grp)
{
  comm_grp_map::iterator it = comm_grp_map_.find(comm);
  if (it == comm_grp_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi group for comm %d for rank %d",
        comm, int(rank_));
  }
  *grp = it->second;
  return MPI_SUCCESS;
}

int
mpi_api::cart_create(MPI_Comm comm_old, int ndims, const int dims[],
                    const int periods[], int reorder, MPI_Comm *comm_cart)
{
  start_mpi_call("MPI_Cart_create");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_create(...)");
  mpi_comm* incommPtr = get_comm(comm_old);
  mpi_comm* outcommPtr = comm_factory_->create_cart(incommPtr, ndims, dims, periods, reorder);
  *comm_cart = add_comm_ptr(outcommPtr);
  return MPI_SUCCESS;
}

int
mpi_api::cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[],
                 int coords[])
{
  start_mpi_call("MPI_Cart_get");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_get(...)");

  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_get: mpi comm did not cast to mpi_comm_cart");

  for (int i = 0; i < maxdims; i++) {
    dims[i] = c->dim(i);
    periods[i] = c->period(i);
  }

  c->set_coords(c->mpi_comm::rank(), coords);

  return MPI_SUCCESS;
}

int
mpi_api::cartdim_get(MPI_Comm comm, int *ndims)
{
  start_mpi_call("MPI_Cartdim_get");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cartdim_get(...)");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cartdim_get: mpi comm did not cast to mpi_comm_cart");
  *ndims = c->ndims();
  return MPI_SUCCESS;
}

int
mpi_api::cart_rank(MPI_Comm comm, const int coords[], int *rank)
{
  start_mpi_call("MPI_Cart_rank");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_rank(...)");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_rank: mpi comm did not cast to mpi_comm_cart");
  *rank = c->rank(coords);
  return MPI_SUCCESS;
}

int
mpi_api::cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source,
                  int *rank_dest)
{
  start_mpi_call("MPI_Cart_shift");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_shift(...)");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_shift: mpi comm did not cast to mpi_comm_cart");
  *rank_source = c->shift(direction, -1 * disp);
  *rank_dest = c->shift(direction, disp);
  return MPI_SUCCESS;
}

int
mpi_api::cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[])
{
  start_mpi_call("MPI_Cart_coords");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_coords(...)");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_coords: mpi comm did not cast to mpi_comm_cart");
  c->set_coords(rank, coords);
  return MPI_SUCCESS;
}


int
mpi_api::comm_split(MPI_Comm incomm, int color, int key, MPI_Comm *outcomm)
{
  start_mpi_call("MPI_Comm_split");
  mpi_api_debug(sprockit::dbg::mpi,
      "MPI_Comm_split(%s,%d,%d) enter",
       comm_str(incomm).c_str(), color, key);
  mpi_comm* incommPtr = get_comm(incomm);
  mpi_comm* outcommPtr = comm_factory_->comm_split(incommPtr, color, key);
  *outcomm = add_comm_ptr(outcommPtr);
  mpi_api_debug(sprockit::dbg::mpi,
      "MPI_Comm_split(%s,%d,%d,*%s) exit",
                comm_str(incomm).c_str(), color, key, comm_str(*outcomm).c_str());
  return MPI_SUCCESS;
}

int
mpi_api::comm_free(MPI_Comm* input)
{
  start_mpi_call("MPI_Comm_free");
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Comm_free(%s)", comm_str(*input).c_str());
  mpi_comm* inputPtr = get_comm(*input);
  comm_map_.erase(*input);
  delete inputPtr;
  *input = MPI_COMM_NULL;
  return MPI_SUCCESS;
}

}
