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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPI_PERSISTENT_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPI_PERSISTENT_H_INCLUDED

#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_request.h>

namespace sstmac {
namespace sumi {

/**
 * Base for persistent operations.
 */
class mpi_api::persistent : public mpi_request
{
 public:
  persistent();

  virtual ~persistent() throw();

  virtual void start() = 0;

  bool is_persistent() const {
    return true;
  }

  bool started_;
};

/**
 * Persistent send operations (send, bsend, rsend, ssend)
 */
class mpi_api::persistent_send : public mpi_api::persistent
{

 public:
  persistent_send(mpi_queue* queue,
                  int count, MPI_Datatype type,
                  int target, int tag, mpi_comm* comm,
                  void* buf);

  virtual ~persistent_send() throw();

  /// Start this request.
  virtual void start();

 private:
  /// The parent object.
  mpi_queue* queue_;

  /// The arguments.
  int count_;
  MPI_Datatype type_;
  int target_;
  int tag_;
  mpi_comm* comm_;
  void* content_;


};

/**
 * Persistent recv operations
 */
class mpi_api::persistent_recv : public mpi_api::persistent
{

 public:
  persistent_recv(mpi_queue* queue, int count, MPI_Datatype type,
                  int sender, int tag, mpi_comm* comm, void* buffer);

  virtual ~persistent_recv() throw();

  /// Start this request.
  virtual void start();

 private:
  /// Parent object.
  mpi_queue* queue_;

  /// The arguments.
  int count_;
  MPI_Datatype type_;
  int sender_;
  int tag_;
  mpi_comm* comm_;
  void* buffer_;

};

}
} // end of namespace sstmac

#endif

