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

#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/common/messages/payload.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

mpi_status* mpi_status::ignore = (mpi_status*) 0x1;

//
// Hello.
//
mpi_status::mpi_status() :
  commid_(-1),
  source_(mpi::any_source),
  dest_(mpi::any_source),
  tag_(mpi::any_tag),
  send_type_(mpi_type::mpi_byte->id),
  recv_type_(mpi_type::mpi_byte->id),
  sent_(0),
  recvd_(0),
  content_(0)
{
}

//
// Copy.
//
mpi_status::mpi_status(const mpi_status &m) :
  commid_(m.commid_),
  source_(m.source_),
  dest_(m.dest_),
  tag_(m.tag_),
  send_type_(m.send_type_),
  recv_type_(m.recv_type_),
  sent_(m.sent_),
  recvd_(m.recvd_),
  content_(m.content())
{
}

//
// Goodbye.
//
mpi_status::~mpi_status() throw()
{
}

//
// Copy.
//
mpi_status*
mpi_status::clone() const
{
  return new mpi_status(*this);
}


}
} // end of namespace sstmac.

