/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>

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

mpi_request::mpi_request(const key::category& cat) :
#if SSTMAC_COMM_SYNC_STATS
 time_sent_(-1),
 time_arrived_(-1),
#endif
 key_(key::construct(cat)),
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

//
// Build me a request.
//
mpi_request*
mpi_request::construct(const key::category& cat)
{
  return new mpi_request(cat);
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

