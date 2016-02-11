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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMMFACTORY_PENDING_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_COMM_MPICOMMFACTORY_PENDING_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_factory.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_data.h>
#include <sstmac/software/process/key.h>

namespace sstmac {
namespace sw {

////
///Manage information associated with pending requests for construction of
////MPI communicators.
////
class mpi_comm_factory::pending  {

 public:
  virtual std::string
  to_string() const {
    return "pending";
  }

  /// Goodbye.
  virtual ~pending() {}

 protected:
  /// Constructor.
  pending(const mpi_comm_id &new_id);

  /// Block current caller.
  void block();

  /// Unblock all callers.
  void unblock();

 protected:
  /// The identifier for the mpi group we're building.
  mpi_comm_id id_;

  /// The group we are using to build from.
  mpi_comm* template_;


};

////
////Supervise calls corresponding to MPI_Comm_create.
////
class mpi_comm_factory::pending_create :
  public mpi_comm_factory::pending
{

 public:
  /// Goodbye.
  virtual ~pending_create() {}

  /// Constructor.
  /// \param new_id is the identifier for the new group.
  pending_create(const mpi_comm_id &new_id);

  /// Get a new communicator for the target group.
  /// Blocks the caller until the group is complete.
  /// All members in the old group must make this call; nodes that
  /// are not in the new group will get mpicomm::null_comm.
  /// \throw value_error if the members don't match earlier requests.
  mpi_comm* build(mpi_comm*caller,
                      const std::vector<mpi_id> &members);

 protected:
  /// The tasks (jobs) to be included in this group.
  std::vector<task_id> target_peers_;

  /// Mapping from node index in the input group to node indices
  /// in the newly created group.
  /// Set to -1 for all nodes that are not members of the new group.
  std::vector<mpi_id> index_mapping_;

  /// The number of processes that have entered the add(...) method.
  size_t added_;

  /// The shared mpicommdata.  Gets constructed when everybody
  /// has called add.
  mpi_comm_data* commdata_;

};

////
/////Supervise calls corresponding to MPI_Comm_split.
/////
class mpi_comm_factory::pending_split : public mpi_comm_factory::pending
{
 public:

  /// Goodbye.
  virtual ~pending_split() {}

  /// Constructor.
  pending_split(const mpi_comm_id &new_id, int maxsize);

  /// Add a participant.  If the call is not complete yet, the caller
  /// gets context-switched out pending completion of the call.
  mpi_comm* build(mpi_comm*caller, int color, int key);

  /// Nested type to manage the key, taskid, and nodeid for each color.
  class keylist  {

   public:
    virtual std::string
    to_string() const {
      return "keylist";
    }

    /// Goodbye.
    virtual ~keylist();

    /// Hello.
    keylist(const mpi_comm_id &id);

    /// Return true if no ranks have been added yet.
    bool
    empty() const;

    /// Return true if shared state has been constructed (no more insert
    /// calls can be made).
    bool
    locked() const;

    /// Test whether the given rank is already defined in the keylist.
    /// Returns true for negative keys (since they correspond to comm_null).
    bool
    contains(int key) const;

    /// Insert a new rank in the keylist.
    /// \throw sprockit::illformed_error if(this->locked() == true).
    /// \throw value_error if(key >= 0 && contains(key) == true).
    void
    insert(int key, const task_id &tid, node_id &nid);

    /// Get a communicator corresponding to the given key.
    /// Locks the keylist (locked() will return false after this call).
    /// \throw uninitializederror if(key >= 0 && contains(key) == false).
    /// \throw value_error if the key is not defined in the keylist.
    mpi_comm*
    get_mpicomm(int key);

   protected:
    /// This is how we store the location of each rank.
    struct location {
      location() :
        the_task(-1),
        final_rank(-1) {
      }
      location(const task_id &tid) :
        the_task(tid) {
      }
      task_id the_task;
      /// This rank gets populated on first call to get_mpicomm,
      /// after which the list is locked.
      mpi_id final_rank;
    };

    /// The nodes (keys) encountered so far.
    /// The keylist is complete whenever the ranks are densely populated
    /// from zero to size-1.
    typedef std::map<int, location> rankmap_t;
    rankmap_t ranks_;

    /// The communicator id.
    mpi_comm_id id_;

    /// When get_mpicomm* is called the first time, this shared state
    /// gets constructed.  After that, no more nodes can be inserted.
    mpi_comm_data* shared_;


  };

  /// This is where the mechanics of aggregating entries by color
  /// and building the communicators takes place.
  std::map<int, keylist*> colors_;

  /// Nodes still pending.
  int pending_;

  /// The maximum number of communicators we're allowed to generate.
  int maxsize_;

  /// The identifier for the next communicator constructed.
  mpi_comm_id current_id_;


};


}
} // end of namespace sstmac

#endif

