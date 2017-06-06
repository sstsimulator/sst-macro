/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#include <sumi-mpi/mpi_comm/mpi_comm_cart.h>

namespace sumi {

/// Hello.
mpi_comm_cart::mpi_comm_cart(
  MPI_Comm id,
  int rank, mpi_group* peers,
  app_id aid, int ndims,
  const int *dims, const int *periods, int reorder) :
  mpi_comm(id, rank, peers, aid, TOPO_CART),
  ndims_(ndims), reorder_(reorder)
{
  for (int i = 0; i < ndims; i++) {
    dims_.push_back(dims[i]);
    periods_.push_back(periods[i]);
  }
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
mpi_comm_cart::rank(const int* coords)
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
      return rank(coords);
    }
    else {
      return mpi_comm::proc_null;
    }

  }
  else if (coords[dir] < 0) {
    if (periods_[dir]) {
      coords[dir] = (dims_[dir] + coords[dir]) % dims_[dir];
      return rank(coords);
    }
    else {
      return mpi_comm::proc_null;
    }
  }
  else {
    return rank(coords);
  }

}

}