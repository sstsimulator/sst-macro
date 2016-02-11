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

#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_request.h>

namespace sstmac {
namespace sw {

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
  /// The function type used to decide what kind of send we do.
  typedef timestamp (mpi_api::*sendfunc_t)
  (int count, mpi_type_id type, mpi_id target,
   mpi_tag tag, mpi_comm* comm, mpi_request* req,
   const payload::const_ptr& content);


 public:
  persistent_send(mpi_api* api,
                  sendfunc_t func, int count, mpi_type_id type,
                  mpi_id target, mpi_tag tag, mpi_comm* comm,
                  void* buf);

  virtual ~persistent_send() throw();

  /// Start this request.
  virtual void start();

 private:
  /// The parent object.
  mpi_api* parent_;
  sendfunc_t func_;

  /// The arguments.
  int count_;
  mpi_type_id type_;
  mpi_id target_;
  mpi_tag tag_;
  mpi_comm* comm_;
  void* content_;


};

/**
 * Persistent recv operations
 */
class mpi_api::persistent_recv : public mpi_api::persistent
{

 public:
  persistent_recv(mpi_api* api, int count, mpi_type_id type,
                  mpi_id target, mpi_tag tag, mpi_comm* comm);

  virtual ~persistent_recv() throw();

  /// Start this request.
  virtual void start();

 private:
  /// Parent object.
  mpi_api* parent_;

  /// The arguments.
  int count_;
  mpi_type_id type_;
  mpi_id dest_;
  mpi_tag tag_;
  mpi_comm* comm_;

};

}
} // end of namespace sstmac

#endif

