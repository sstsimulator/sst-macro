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

#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sstmac/software/process/key.h>

namespace sumi {

collective_op_base::collective_op_base(mpi_comm* cm) :
  comm(cm), packed_send(false), packed_recv(false),
  tag(cm->next_collective_tag())
{
}

collective_op::collective_op(int scnt, int rcnt, mpi_comm *cm) :
    collective_op_base(cm)
{
  sendcnt = scnt;
  recvcnt = rcnt;
}

collectivev_op::collectivev_op(int scnt, int* recvcnts, int* rd, mpi_comm* cm) :
    collective_op_base(cm),
    recvcounts(recvcnts),
    sendcounts(0), rdisps(rd)
{
  sendcnt = scnt;
  recvcnt = 0;
}

collectivev_op::collectivev_op(int* sendcnts, int* sd, int rcnt, mpi_comm* cm) :
    collective_op_base(cm),
    sendcounts(sendcnts),
    sdisps(sd)
{
  sendcnt = 0;
  recvcnt = rcnt;
}

collectivev_op::collectivev_op(int* sendcnts, int* sd,
                               int* recvcnts, int *rd, mpi_comm* cm) :
  collective_op_base(cm),
  recvcounts(recvcnts), sendcounts(sendcnts),
  sdisps(sd), rdisps(rd)
{
  sendcnt = 0;
  recvcnt = 0;
}

collective_op::collective_op(int count, mpi_comm* cm) :
  collective_op_base(cm)
{
  sendcnt = count;
  recvcnt = count;
}

mpi_request::mpi_request(op_type_t ty, const category& cat) :
 key_(key::construct(cat)),
 optype_(ty),
 complete_(false),
 cancelled_(false),
 persistent_op_(nullptr),
 collective_op_(nullptr)
{
}

mpi_request::~mpi_request()
{
  delete key_;
  if (persistent_op_) delete persistent_op_;
  //do not delete - deleted elsewhere
  //if (collective_op_) delete collective_op_;
}

void
mpi_request::complete(const mpi_message::ptr& msg)
{
  if (!cancelled_) {
    msg->build_status(&stat_);
  }
  complete();
}

std::string
mpi_request::type_str() const 
{
  if (is_persistent()){
    return "persistent";
  } else if (is_collective()){
    return "collective";
  } else {
    return "regular";
  }
}

}