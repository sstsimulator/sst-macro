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

#include <sstmac/libraries/mpi/mpi_api_persistent.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/software/process/key.h>
#include <sprockit/errors.h>
#include <sstmac/libraries/mpi/mpi_payload.h>

namespace sstmac {
namespace sw {


mpi_api::persistent::persistent() :
  mpi_request(default_key_category),
  started_(false)
{
}

mpi_api::persistent::~persistent() throw()
{
}

mpi_api::persistent_send::
persistent_send(mpi_api* api,
                sendfunc_t func, int count, mpi_type_id type,
                mpi_id target, mpi_tag  tag, mpi_comm* comm,
                void* buf) :
  parent_(api), func_(func), count_(count),
  type_(type), target_(target), tag_(tag), comm_(comm), content_(buf)
{
  if(parent_ == NULL || func_ == NULL) {
    spkt_throw(sprockit::illformed_error,
        "mpiapi::persistent_send: invalid parent info.");
  }

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
  payload::const_ptr load;
  started_ = true;
  if(content_) {
    mpi_type* type_obj = parent_->type_from_id(type_);
    load = new mpi_payload(content_, type_obj, count_, false);
  }
  ((*parent_).*func_)(count_, type_, target_, tag_, comm_,
                      this, load);
}


mpi_api::persistent_recv::
persistent_recv(mpi_api* api, int count, mpi_type_id type,
                mpi_id dest, mpi_tag tag, mpi_comm* comm) :
  parent_(api), count_(count), type_(type),
  dest_(dest), tag_(tag), comm_(comm)
{
  if(parent_ == NULL) {
    spkt_throw(sprockit::illformed_error, "mpiapi::persistentrecv: invalid parent info.");
  }

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
  parent_->do_irecv(count_, type_, dest_, tag_, comm_, this);
}


}
} // end of namespace sstmac

