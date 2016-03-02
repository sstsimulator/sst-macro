#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_comm/mpi_comm_cart.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {


int
mpi_api::comm_dup(MPI_Comm input, MPI_Comm *output)
{
  SSTMACBacktrace("MPI_Comm_dup");
  mpi_comm* inputPtr = get_comm(input);
  mpi_comm* outputPtr = comm_factory_->comm_dup(inputPtr);
  *output = add_comm_ptr(outputPtr);
  return MPI_SUCCESS;
}

int
mpi_api::comm_create(MPI_Comm input, MPI_Group group, MPI_Comm *output)
{
  SSTMACBacktrace("MPI_Comm_create");
  mpi_comm* inputPtr = get_comm(input);
  mpi_group* groupPtr = get_group(group);
  *output = add_comm_ptr(comm_factory_->comm_create(inputPtr, groupPtr));
  return MPI_SUCCESS;
}

int
mpi_api::cart_create(MPI_Comm comm_old, int ndims, const int dims[],
                    const int periods[], int reorder, MPI_Comm *comm_cart)
{
  SSTMACBacktrace("MPI_Cart_create");
  mpi_comm* incommPtr = get_comm(comm_old);
  mpi_comm* outcommPtr = comm_factory_->create_cart(incommPtr, ndims, dims, periods, reorder);
  *comm_cart = add_comm_ptr(outcommPtr);
  return MPI_SUCCESS;
}

int
mpi_api::cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[],
                 int coords[])
{
  SSTMACBacktrace("MPI_Cart_get");

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
  SSTMACBacktrace("MPI_Cartdim_get");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cartdim_get: mpi comm did not cast to mpi_comm_cart");
  *ndims = c->ndims();
  return MPI_SUCCESS;
}

int
mpi_api::cart_rank(MPI_Comm comm, const int coords[], int *rank)
{
  SSTMACBacktrace("MPI_Cart_rank");
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
  SSTMACBacktrace("MPI_Cart_shift");
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
  SSTMACBacktrace("MPI_Cart_coords");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_coords: mpi comm did not cast to mpi_comm_cart");
  c->set_coords(rank, coords);
  return MPI_SUCCESS;
}


int
mpi_api::comm_split(MPI_Comm incomm, int color, int key, MPI_Comm *outcomm)
{
  SSTMACBacktrace("MPI_Comm_split");
  mpi_comm* incommPtr = get_comm(incomm);
  mpi_comm* outcommPtr = comm_factory_->comm_split(incommPtr, color, key);
  *outcomm = add_comm_ptr(outcommPtr);
  return MPI_SUCCESS;
}

int
mpi_api::comm_free(MPI_Comm* input)
{
  SSTMACBacktrace("MPI_Comm_free");
  mpi_comm* inputPtr = get_comm(*input);
  delete inputPtr;
  *input = MPI_COMM_NULL;
  return MPI_SUCCESS;
}

}
