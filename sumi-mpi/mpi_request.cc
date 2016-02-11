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

namespace sstmac {
namespace sumi {

mpi_request::mpi_request(const sw::key::category& cat) :
 key_(sw::key::construct(cat)),
 complete_(false),
 cancelled_(false)
{
}

mpi_request::~mpi_request()
{
  delete key_;
}

//
// Build me a request.
//
mpi_request*
mpi_request::construct(const sw::key::category& cat)
{
  return new mpi_request(cat);
}

void
mpi_request::complete(const mpi_message::ptr& msg)
{
  if (msg && !cancelled_) {
    msg->build_status(&stat_);
  }
  complete_ = true;
}

}
} //end of namespace sstmac

