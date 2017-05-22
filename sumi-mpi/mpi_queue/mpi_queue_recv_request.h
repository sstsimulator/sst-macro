/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_RECVREQUEST_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_RECVREQUEST_H_INCLUDED

#include <sumi-mpi/mpi_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_fwd.h>
#include <sumi-mpi/mpi_message.h>
#include <sstmac/common/event_location.h>

namespace sumi {


/**
 * A nested type to handle individual mpi receive requests.
 */
class mpi_queue_recv_request  {
  friend class mpi_queue;
  friend class rendezvous_get;
  friend class eager1;
  friend class eager1_doublecpy;
  friend class eager0;
  friend class eager1_singlecpy;

 public:
  /// Hello.
  mpi_queue_recv_request(mpi_request* key, mpi_queue* queue,
                         int count,
                         MPI_Datatype type, int source, int tag,
                         MPI_Comm comm, void* buffer);

  /// Goodbye.
  ~mpi_queue_recv_request();

  /// Do we match the given message?
  bool matches(const mpi_message::ptr& msg);

  void set_seqnum(int seqnum) {
    seqnum_ = seqnum;
  }

  mpi_request* req() const {
    return key_;
  }

  bool is_cancelled() const;

 private:
  /// The queue to whom we belong.
  mpi_queue* queue_;

  /// The parameters I will be matching on.
  int source_;
  int tag_;
  MPI_Comm comm_;
  int seqnum_;

  /** Where data will end up in its final form */
  void* final_buffer_;
  /** Temporary buffer that initially receives data.
   *  Needed for non-contiguous types */
  char* recv_buffer_;

  /// The parameters I will not be matching on, but are good error checks.
  int count_;
  mpi_type* type_;
  mpi_request* key_;
};

}

#endif