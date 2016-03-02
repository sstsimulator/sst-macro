/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sumi-mpi/mpi_api_persistent.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/key.h>
#include <sprockit/errors.h>

namespace sumi {


mpi_api::persistent::persistent() :
  mpi_request(default_key_category),
  started_(false)
{
}

mpi_api::persistent::~persistent() throw()
{
}

mpi_api::persistent_send::
persistent_send(mpi_queue* queue, int count, MPI_Datatype type,
                int target, int tag, mpi_comm* comm,
                void* buf) :
  queue_(queue), count_(count),
  type_(type), target_(target), tag_(tag), comm_(comm), content_(buf)
{
}


mpi_api::persistent_send::~persistent_send() throw()
{
}

//
// Start this request.
//
void
mpi_api::persistent_send::start()
{
  started_ = true;
  queue_->send(this, count_, type_, target_, tag_, comm_, content_);
}


mpi_api::persistent_recv::
persistent_recv(mpi_queue* queue, int count, MPI_Datatype type,
                int sender, int tag, mpi_comm* comm, void* buffer) :
  queue_(queue), count_(count), type_(type),
  sender_(sender), tag_(tag), comm_(comm), buffer_(buffer)
{
}

mpi_api::persistent_recv::~persistent_recv() throw()
{
}

//
// Start this request.
//
void mpi_api::persistent_recv::start()
{
  started_ = true;
  queue_->recv(this, count_, type_, sender_, tag_, comm_, buffer_);
}


}

