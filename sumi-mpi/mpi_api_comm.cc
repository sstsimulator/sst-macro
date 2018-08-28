/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_comm/mpi_comm_cart.h>
#include <sprockit/stl_string.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

#define start_comm_call(fxn,comm) \
  auto call_start_time = (uint64_t)os_->now().usec(); \
  start_mpi_call(fxn); \
  mpi_api_debug(sprockit::dbg::mpi, "%s(%s) start", #fxn, comm_str(comm).c_str())

#define finish_comm_call(fxn,input,output) \
  mpi_api_debug(sprockit::dbg::mpi, "%s(%s,*%s) finish", \
                #fxn, comm_str(input).c_str(), comm_str(*output).c_str());


namespace sumi {

int
mpi_api::comm_dup(MPI_Comm input, MPI_Comm *output)
{
  check_init();
  start_comm_call(MPI_Comm_dup,input);
  mpi_comm* inputPtr = get_comm(input);
  mpi_comm* outputPtr = comm_factory_.comm_dup(inputPtr);
  add_comm_ptr(outputPtr, output);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_dup(%s,*%s) finish",
                comm_str(input).c_str(), comm_str(*output).c_str());
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.mpi_comm_dup(comm_world()->rank(), call_start_time,
                              (uint64_t)os_->now().usec(), input, *output);
#endif
  return MPI_SUCCESS;
}

int
mpi_api::comm_create_group(MPI_Comm comm, MPI_Group group, int tag, MPI_Comm *newcomm)
{
  check_init();
  start_comm_call(MPI_Comm_create_group,comm);
  mpi_comm* inputPtr = get_comm(comm);
  mpi_group* groupPtr = get_group(group);
  mpi_comm* outputPtr = comm_factory_.comm_create_group(inputPtr, groupPtr);
  add_comm_ptr(outputPtr, newcomm);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_create_group(%s,*%s) finish",
                comm_str(comm).c_str(), comm_str(*newcomm).c_str());
  end_api_call();
  //okay, this is really complicated

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_) {
   // TODO, not implemented yet
  }
#endif
  return MPI_SUCCESS;
}

int
mpi_api::comm_size(MPI_Comm comm, int *size)
{
  start_comm_call(MPI_Comm_size,comm);
  *size = get_comm(comm)->size();
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Comm_size");
#endif
  return MPI_SUCCESS;
}

int
mpi_api::comm_create(MPI_Comm input, MPI_Group group, MPI_Comm *output)
{
  start_comm_call(MPI_Comm_create,input);
  mpi_comm* inputPtr = get_comm(input);
  mpi_group* groupPtr = get_group(group);
  add_comm_ptr(comm_factory_.comm_create(inputPtr, groupPtr), output);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_create(%s,%d,*%s)",
                comm_str(input).c_str(), group, comm_str(*output).c_str());
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.mpi_comm_create(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), input, group, *output);
#endif
  return MPI_SUCCESS;
}

void
mpi_api::comm_create_with_id(MPI_Comm input, MPI_Group group, MPI_Comm new_comm)
{
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_create_with_id(%s,%d,%d)",
                comm_str(input).c_str(), group, new_comm);
  mpi_group* groupPtr = get_group(group);
  mpi_comm* inputPtr = get_comm(input);
  int new_rank = groupPtr->rank_of_task(inputPtr->my_task());
  if (new_rank != -1){ //this is actually part of the group
    mpi_comm* newCommPtr = new mpi_comm(new_comm, new_rank, groupPtr, library::sid_.app_);
    add_comm_ptr(newCommPtr, &new_comm);
  }
#ifdef SSTMAC_OTF2_ENABLED
  // treate like ::comm_create
  if(otf2_enabled_) {
    auto start_time = (uint64_t)os_->now().usec();
    otf2_writer_.mpi_comm_create(comm_world()->rank(), start_time, start_time, input, group, new_comm);
  }
#endif
}

int
mpi_api::comm_group(MPI_Comm comm, MPI_Group* grp)
{
  mpi_comm* commPtr = get_comm(comm);
  *grp = commPtr->group()->id();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_) {
    auto start_time = (uint64_t)os_->now().usec();
    otf2_writer_.mpi_comm_group(comm_world()->rank(), start_time, start_time, comm, *grp);
  }
#endif

  return MPI_SUCCESS;
}

int
mpi_api::cart_create(MPI_Comm comm_old, int ndims, const int dims[],
                    const int periods[], int reorder, MPI_Comm *comm_cart)
{
  start_comm_call(MPI_Cart_create,comm_old);
  mpi_comm* incommPtr = get_comm(comm_old);
  mpi_comm* outcommPtr = comm_factory_.create_cart(incommPtr, ndims, dims, periods, reorder);
  add_comm_ptr(outcommPtr, comm_cart);
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  // Cart actions are not fully implemented by the writer.
  // TODO add warning
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Cart_create");
#endif

  return MPI_SUCCESS;
}

int
mpi_api::cart_get(MPI_Comm comm, int maxdims, int dims[], int periods[],
                 int coords[])
{
  start_comm_call(MPI_Cart_get,comm);

  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_get: mpi comm did not cast to mpi_comm_cart");

  for (int i = 0; i < maxdims; i++) {
    dims[i] = c->dim(i);
    periods[i] = c->period(i);
  }

  c->set_coords(c->mpi_comm::rank(), coords);
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_) {
    auto start_time = (uint64_t)os_->now().usec();
    otf2_writer_.generic_call(comm_world()->rank(), start_time, start_time, "MPI_Cart_get");
  }
#endif

  return MPI_SUCCESS;
}

int
mpi_api::cartdim_get(MPI_Comm comm, int *ndims)
{
  start_comm_call(MPI_Cartdim_get,comm);
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cartdim_get: mpi comm did not cast to mpi_comm_cart");
  *ndims = c->ndims();
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Cartdim_get");
#endif

  return MPI_SUCCESS;
}

int
mpi_api::cart_rank(MPI_Comm comm, const int coords[], int *rank)
{
  start_comm_call(MPI_Cart_rank,comm);
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_rank: mpi comm did not cast to mpi_comm_cart");
  *rank = c->rank(coords);
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Cart_rank");
#endif

  return MPI_SUCCESS;
}

int
mpi_api::cart_shift(MPI_Comm comm, int direction, int disp, int *rank_source,
                  int *rank_dest)
{
  start_comm_call(MPI_Cart_shift,comm);
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_shift: mpi comm did not cast to mpi_comm_cart");
  *rank_source = c->shift(direction, -1 * disp);
  *rank_dest = c->shift(direction, disp);
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Cart_shift");
#endif

  return MPI_SUCCESS;
}

int
mpi_api::cart_coords(MPI_Comm comm, int rank, int maxdims, int coords[])
{
  start_comm_call(MPI_Cart_coords,comm);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_coords(...)");
  mpi_comm* incommPtr = get_comm(comm);
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, incommPtr,
    "mpi_api::cart_coords: mpi comm did not cast to mpi_comm_cart");
  c->set_coords(rank, coords);
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Cart_coords");
#endif

  return MPI_SUCCESS;
}


int
mpi_api::comm_split(MPI_Comm incomm, int color, int key, MPI_Comm *outcomm)
{
  start_comm_call(MPI_Comm_split,incomm);
  mpi_comm* incommPtr = get_comm(incomm);
  mpi_comm* outcommPtr = comm_factory_.comm_split(incommPtr, color, key);
  add_comm_ptr(outcommPtr, outcomm);
  mpi_api_debug(sprockit::dbg::mpi,
      "MPI_Comm_split(%s,%d,%d,*%s) exit",
      comm_str(incomm).c_str(), color, key, comm_str(*outcomm).c_str());

  //but also assign an id to the underlying group
  if (outcommPtr->id() != MPI_COMM_NULL){
    outcommPtr->group()->set_id(group_counter_++);
  }

  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.mpi_comm_split(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), incomm, key, color, *outcomm);
#endif


  return MPI_SUCCESS;
}

int
mpi_api::comm_free(MPI_Comm* input)
{
  start_comm_call(MPI_Comm_free,*input);
  mpi_comm* inputPtr = get_comm(*input);
  comm_map_.erase(*input);
  if (inputPtr->delete_group()){
    grp_map_.erase(inputPtr->group()->id());
  }
  delete inputPtr;
  *input = MPI_COMM_NULL;
  end_api_call();

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_)
    otf2_writer_.generic_call(comm_world()->rank(), call_start_time, (uint64_t)os_->now().usec(), "MPI_Comm_free");
#endif


  return MPI_SUCCESS;
}

int
mpi_api::comm_get_attr(MPI_Comm, int comm_keyval, void* attribute_val, int *flag)
{
  /**
  if (comm_keyval == MPI_TAG_UB){
    *attribute_val = std::numeric_limits<int>::max();
    *flag = 1;
  } else {
    *flag = 0;
  }
  */
  *flag = 0;

#ifdef SSTMAC_OTF2_ENABLED
  if(otf2_enabled_) {
    auto start_time = (uint64_t)os_->now().usec();
    otf2_writer_.generic_call(comm_world()->rank(), start_time, start_time, "MPI_Comm_get_attr");
  }
#endif

  return MPI_SUCCESS;
}

}
