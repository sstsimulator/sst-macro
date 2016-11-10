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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIREQUEST_H_INCLUDED

#include <sstmac/software/process/key.h>
#include <sumi/collective.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_comm/mpi_comm_fwd.h>
#include <sstmac/common/sstmac_config.h>


namespace sumi {

using sstmac::sw::key;

class pt2pt_delay_histograms
{

};

/**
 * Persistent send operations (send, bsend, rsend, ssend)
 */
class persistent_op
{
 public:
  typedef enum {
    Send,
    Recv
  } op_type_t;
  /// The arguments.
  int count;
  MPI_Datatype datatype;
  MPI_Comm comm;
  op_type_t optype;
  int partner;
  int tag;
  void* content;
};

struct collective_op_base
{

  bool packed_send;
  bool packed_recv;
  void* sendbuf;
  void* recvbuf;
  void* tmp_sendbuf;
  void* tmp_recvbuf;
  int tag;
  MPI_Op op;
  mpi_type* sendtype;
  mpi_type* recvtype;
  collective::type_t ty;
  mpi_comm* comm;
  int sendcnt;
  int recvcnt;
  int root;

 protected:
  collective_op_base(mpi_comm* cm);

};

struct collective_op : public collective_op_base
{
  collective_op(int count, mpi_comm* comm);
  collective_op(int sendcnt, int recvcnt, mpi_comm* comm);


};

struct collectivev_op : public collective_op_base
{
  collectivev_op(int scnt, int* recvcnts, int* disps, mpi_comm* comm);
  collectivev_op(int* sendcnts, int* disps, int rcnt, mpi_comm* comm);
  collectivev_op(int* sendcnts, int* sdisps,
                 int* recvcnts, int* rdisps, mpi_comm* comm);

  int* recvcounts;
  int* sendcounts;
  int* sdisps;
  int* rdisps;
  int size;
};

class mpi_request  {
  // ------- constructor / boost stuff -------------//

 public:
  mpi_request(const key::category& cat);

  std::string
  to_string() const {
    return "mpirequest";
  }

  std::string
  type_str() const;

  static mpi_request*
  construct(const key::category& cat);
  // --------------------------------------//

  ~mpi_request();

  void
  complete(const mpi_message::ptr& msg);

  bool
  is_complete() const {
    return complete_;
  }

  void
  cancel() {
    cancelled_ = true;
    complete();
  }

  void
  complete() {
    complete_ = true;
  }

  void
  set_complete(bool flag){
    complete_ = flag;
  }

  void
  set_persistent(persistent_op* op) {
    persistent_op_ = op;
  }

  persistent_op*
  persistent_data() const {
    return persistent_op_;
  }

  void
  set_collective(collective_op_base* op) {
    collective_op_ = op;
  }

  collective_op_base*
  collective_data() const {
    return collective_op_;
  }

  const MPI_Status&
  status() const {
    return stat_;
  }

  key*
  get_key() const {
    return key_;
  }

  bool
  is_cancelled() const {
    return cancelled_;
  }

  bool
  is_persistent() const {
    return persistent_op_;
  }

  bool
  is_collective() const {
    return collective_op_;
  }

 private:
  MPI_Status stat_;
  key* key_;
  bool complete_;
  bool cancelled_;

  persistent_op* persistent_op_;
  collective_op_base* collective_op_;

#if SSTMAC_COMM_SYNC_STATS
 public:
  void
  set_time_sent(double now){
    time_sent_ = now;
  }

  void
  set_time_arrived(double now){
    time_arrived_ = now;
  }

  double
  time_sent() const {
    return time_sent_;
  }

  double
  time_arrived() const {
    return time_arrived_;
  }

 private:
  double time_sent_;
  double time_arrived_;
#endif

};

}

#endif

