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

#include <fake/fake_transport.h>
#include <sprockit/sim_parameters.h>
#include <unistd.h>

namespace sumi {

static message::ptr fake_msg;

message::ptr
fake_transport::block_until_message()
{
  sleep(1);
  if (!fake_msg){
    fake_msg = new message;
    fake_msg->set_class_type(message::fake);
  }
  return fake_msg;
}

collective_done_message::ptr
fake_transport::collective_block(collective::type_t ty, int tag)
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never block");
}

void
fake_transport::delayed_transport_handle(const message::ptr &msg)
{
  handle(msg);
}

void
fake_transport::cq_notify()
{
  //do nothng
}

void
fake_transport::schedule_ping_timeout(pinger *pnger, double to)
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never ping or have failed pings");
}

void
fake_transport::schedule_next_heartbeat()
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never ping or have failed pings");
}

void
fake_transport::do_smsg_send(int dst, const message::ptr &msg)
{
  sends_.push_back(msg);
}

fake_transport::fake_transport(sprockit::sim_parameters *params) :
  transport(params)
{
  nproc_ = params->get_int_param("fake_transport_nproc");
  rank_ = params->get_int_param("fake_transport_rank");
}

void
fake_transport::do_rdma_get(int src, const message::ptr &msg)
{
  rdma_gets_.push_back(msg);
}

void
fake_transport::do_rdma_put(int dst, const message::ptr &msg)
{
  rdma_puts_.push_back(msg);
}

void
fake_transport::do_nvram_get(int src, const message::ptr &msg)
{
  nvram_gets_.push_back(msg);
}

void
fake_transport::simulate_vote(int context, const thread_safe_set<int> &failures)
{
  votes_done_[context] = vote_result(1, failures);
}

message::ptr
fake_transport::pop_message(std::list<message::ptr> &msglist)
{
  if (msglist.empty()){
    return message::ptr();
  } else {
    message::ptr ret = msglist.front();
    msglist.pop_front();
    return ret;
  }
}

void
fake_transport::do_send_terminate(int dst)
{
  spkt_throw(sprockit::unimplemented_error,
    "fake transport should not send terminates");
}

void
fake_transport::do_send_ping_request(int dst)
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never ping or have failed pings");
}

void
fake_transport::go_die()
{
  spkt_throw(sprockit::unimplemented_error,
    "fake transport should never die");
}

void
fake_transport::go_revive()
{
  spkt_throw(sprockit::unimplemented_error,
    "fake transport should go revive");
}

}