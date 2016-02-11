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
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_data.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/software/process/key.h>
#include <sstmac/common/node_address.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

//
// Base constructor.
//
mpi_comm_factory::pending::pending(const mpi_comm_id &new_id)
  :  id_(new_id)
{
}

//
// Block current caller.
//
void
mpi_comm_factory::pending::block()
{
}

//
// Unblock all callers.
//
void
mpi_comm_factory::pending::unblock()
{
}

//
// Construct a new comm_create handler.
//
mpi_comm_factory::pending_create::pending_create(const mpi_comm_id &new_id) :
  pending(new_id)
{

}

//
// Add the given caller to the membership.
//
mpi_comm* mpi_comm_factory::pending_create::
build(mpi_comm*caller, const std::vector<mpi_id> &members)
{
  spkt_throw(sprockit::unimplemented_error,
            "mpicommfactory::pending_create::build: not implemented");
}

//
// Construct a new handler for comm_split.
//
mpi_comm_factory::pending_split::pending_split(const mpi_comm_id &new_id,
    int maxsize) :
  pending(new_id), pending_(-1), maxsize_(maxsize), current_id_(new_id)
{

}

//
// Add the given caller to the membership.
//
mpi_comm* mpi_comm_factory::pending_split::build(mpi_comm*caller,
    int color, int key)
{
  if(colors_.empty()) {
    // First one in.  Initialize stuff.
    template_ = caller;
    pending_ = caller->size();
  }
  else {
    if(caller->id() != template_->id()) {
      spkt_throw(sprockit::value_error,
                "mpicommfactory::pending_split: Not the correct input communicator");
    }
  }
  // If this is the first time we see this color, we need to assign
  // a communicator id.
  if(! colors_[color]) {
    if(current_id_ >= this->id_ + maxsize_) {
      spkt_throw(sprockit::value_error,
                "mpicommfactory::pending_split::build: MPI communicator index exceeded allowed bounds");
    }
    colors_[color] = new keylist(current_id_);
    current_id_++;
  }
  // Add this rank to the colored communicator.
  node_id nid = caller->my_node();
  colors_[color]->insert(key, caller->my_task(), nid);
  // Decide whether we're done
  --pending_;
  if(pending_ > 0) {
    // Still waiting for others to join the collective.
    this->block();
  }
  else {
    // Done. Shared data gets constructed by the keylist when it's needed.
    // Set all blocked contexts free
    this->unblock();
  }
  // And now we are no longer context-switched out.
  // Note that if this peer has key less than zero, it is inactive
  // and gets comm_null as a return value.
  mpi_comm* retval = colors_[color]->get_mpicomm(key);
  //dbg_ << "mpicommfactory::pending_split::build:  Returning "
  //     << retval->to_string() << " for caller " << caller->to_string()
  //     << " and key " << key << "\n";
  // All done.
  return retval;
}



}
} // end of namespace sstmac

