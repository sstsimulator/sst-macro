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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_RMA_MESSAGE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_RMA_MESSAGE_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>

namespace sstmac {
namespace sw {

class mpi_rma_message : public mpi_message
{
 public:
  typedef enum {
    get, put, acc
  } rmatype_t;

  struct op_info {
    int count_; //number of bytes
    long long disp_;
    rmatype_t type_;
    long epoch_;
    mpi_id target_;
    mpi_op* op_;
  };

 public:
  mpi_rma_message(const std::string& libn, 
                  int64_t envelope, int64_t mintrans, int count,
                  mpi_type_id type, int type_packed_size,
                  mpi_id source, mpi_id dest, mpi_comm_id commid,
                  int seqnum, mpi_message::id msgid, category cat,
                  task_id source_task, task_id dest_task, app_id aid,
                  content_type_t content_type, const payload::const_ptr& content,
                  mpi_protocol* protocol, rmatype_t rmat,
                  int win, const op_info &info);

  mpi_rma_message(content_type_t content_type, rmatype_t rmat, int win,
                  const op_info &info);

  mpi_rma_message(const payload::const_ptr& content,
                  content_type_t content_type, rmatype_t rmat, int win,
                  const op_info &info);

  /// Goodbye.
  virtual
  ~mpi_rma_message() throw () {
  }

  virtual mpi_message*
  deep_clone() const;

  virtual std::string
  to_string() const;

  int
  winid() const {
    return win_;
  }

  rmatype_t
  get_rmatype() const {
    return rmatype_;
  }
  op_info
  get_opinfo() const {
    return info_;
  }

  long
  epoch() const {
    return info_.epoch_;
  }

  virtual void
  build_status(mpi_status* stat) const;

 protected:
  rmatype_t rmatype_;
  int win_;
  op_info info_;

};

}
} // end of namespace sstmac.

#endif

