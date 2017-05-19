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

#ifndef sumi_message_h
#define sumi_message_h

#include <sstmac/hardware/network/network_message.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/key_fwd.h>
#include <sstmac/common/messages/library_message.h>
#include <sstmac/libraries/sumi/message_fwd.h>
#include <sumi/message_fwd.h>

namespace sstmac {

class transport_message :
  public ::sstmac::hw::network_message,
  public ::sstmac::library_interface
{
   ImplementSerializable(transport_message)

 public:
  transport_message(){} //needed for serialization

  transport_message(
     const std::string& libname,
     sw::app_id aid,
     const sumi::message_ptr& msg,
     long byte_length)
   : library_interface(libname),
      network_message(aid, byte_length),
      payload_(msg)
  {
  }

  virtual void serialize_order(serializer& ser) override;

  sumi::message_ptr payload() const {
    return payload_;
  }

  std::string to_string() const override;

  int dest_rank() const {
    return dest_;
  }

  void set_dest_rank(int dest) {
    dest_ = dest;
  }

  int src_rank() const {
    return src_;
  }

  void set_src_rank(int src) {
    src_ = src;
  }

  void set_apps(int src, int dst){
    src_app_ = src;
    dest_app_ = dst;
  }

  int src_app() const {
    return src_app_;
  }

  int dest_app() const {
    return dest_app_;
  }

  virtual void put_on_wire() override;

  ::sstmac::hw::network_message* clone_injection_ack() const override;

 protected:
  void clone_into(transport_message* cln) const;

  void reverse() override;

 private:
  sumi::message_ptr payload_;
  int src_;
  int dest_;
  int src_app_;
  int dest_app_;


};


}

#endif // MESSAGE_H