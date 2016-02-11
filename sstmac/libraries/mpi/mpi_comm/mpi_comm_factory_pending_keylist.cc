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

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_factory_pending.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

//
// Hello.
//
mpi_comm_factory::pending_split::keylist::keylist(const mpi_comm_id &id) :
  id_(id)
{}

//
// Goodbye.
//
mpi_comm_factory::pending_split::keylist::~keylist()
{
}

//
// Return true if no ranks have been added yet.
//
bool mpi_comm_factory::pending_split::keylist::empty() const
{
  return ranks_.empty();
}

//
// Return true if shared state has been constructed.
//

bool mpi_comm_factory::pending_split::keylist::locked() const
{
  return (shared_ != 0);
}

//
// Test whether the given rank is already defined in the keylist.
//
bool mpi_comm_factory::pending_split::keylist::contains(int key) const
{
  return (key < 0 || (ranks_.find(key) != ranks_.end()));
}

//
// Insert a new rank in the keylist.
//
void mpi_comm_factory::pending_split::keylist::
insert(int key, const task_id &tid, node_id &nid
      )
{
  if(this->locked())
    throw sprockit::illformed_error("mpicommfactory::pending_split::keylist::insert:  "
                          "Keylist is locked -- no more insertions.");
  if(key >= 0) {
    if(this->contains(key))
      throw sprockit::value_error("mpicommfactory::pending_split::keylist::insert:  "
                        "Attempt to re-define an existing key.");
  }
}

//
// Get a communicator corresponding to the given key.
//
mpi_comm* mpi_comm_factory::pending_split::keylist::get_mpicomm(int key)
{
  throw sprockit::unimplemented_error("mpicommfactory::pending_split::keylist::get_mpicomm* not implemented");
}

}
} // end of namespace sstmac

