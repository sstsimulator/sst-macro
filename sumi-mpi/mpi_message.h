/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIMESSAGE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIMESSAGE_H_INCLUDED

#include <sstmac/common/sstmac_config.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_types/mpi_type.h>
#include <sumi-mpi/mpi_integers.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>

#include <sstmac/software/process/operating_system_fwd.h>
#include <sumi/message.h>
#include <sprockit/thread_safe_new.h>

namespace sumi {

class mpi_message final :
  public sumi::protocol_message,
  public sprockit::thread_safe_new<mpi_message>
{
  ImplementSerializable(mpi_message)

 public:
  template <class... Args>
  mpi_message(int src_rank, int dst_rank,
              MPI_Datatype type, int tag, MPI_Comm commid, int seqnum,
              int count, int type_size, void* partner_buf, int protocol,
              Args&&... args) :
    sumi::protocol_message(count, type_size, partner_buf, protocol,
                           std::forward<Args>(args)...),
    src_rank_(src_rank),
    dst_rank_(dst_rank),
    type_(type),
    tag_(tag),
    commid_(commid),
    seqnum_(seqnum)
  {
  }

  std::string to_string() const override;

  ~mpi_message() throw ();

  sumi::mpi_message* clone_me() const {
    mpi_message* cln = new mpi_message(*this);
    return cln;
  }

  sstmac::hw::network_message* clone_injection_ack() const override {
    auto* msg = clone_me();
    msg->convert_to_ack();
    return msg;
  }

  void serialize_order(sstmac::serializer& ser) override;

  MPI_Datatype type() const {
    return type_;
  }

  int tag() const {
    return tag_;
  }

  MPI_Comm comm() const {
    return commid_;
  }

  int seqnum() const {
    return seqnum_;
  }

  int src_rank() const {
    return src_rank_;
  }

  int dst_rank() const {
    return dst_rank_;
  }

  void build_status(MPI_Status* stat) const;

 protected:
  //void clone_into(mpi_message* cln) const;

 private:
  mpi_message(){} //for serialization

  int src_rank_;
  int dst_rank_;
  MPI_Datatype type_;
  int tag_;
  MPI_Comm commid_;
  int seqnum_;

};

}

#endif
