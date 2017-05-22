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

#ifndef sumi_active_msg_transport_h
#define sumi_active_msg_transport_h

#include <sumi/monitor.h>
#include <sumi/timeout.h>
#include <sumi/message.h>
#include <sumi/collective.h>
#include <sumi/transport.h>
#include <sumi/comm_functions.h>

namespace sumi {

class active_msg_transport :
  public transport
{
 public:
  void
  cq_notify(){} //no op

  void
  delayed_transport_handle(const message::ptr &msg){
    handle(msg);
  }

  void
  schedule_ping_timeout(pinger *pnger, double to){}

  double
  wall_time() const;

  void
  schedule_next_heartbeat();

  virtual collective_done_message::ptr
  collective_block(collective::type_t ty, int tag);

  message::ptr
  block_until_message();

  message::ptr
  block_until_message(double timeout);

  void
  init();

  typedef enum {
   i_am_alive,
   i_am_dead
  } ping_status_t;

 protected:
  void
  maybe_do_heartbeat();

  active_msg_transport();

  char*
  allocate_message_buffer(const message::ptr& msg, int& size);

  message::ptr
  deserialize(char* buf);

  message::ptr
  free_message_buffer(void* buf);

  char* allocate_smsg_buffer();

  void free_smsg_buffer(void* buf);

 private:
  double time_zero_;

  double next_heartbeat_;

 protected:
  virtual void block_inner_loop() = 0;

  char* smsg_buffer_;

  const int smsg_buffer_size_;

  std::list<char*> smsg_buffer_pool_;
};

}

#endif