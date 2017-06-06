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

#include <sumi/ping.h>
#include <sumi/transport.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

using namespace sprockit::dbg;

RegisterKeywords(
"ping_timeout",
"activity_monitor",
);

namespace sumi {

pinger::pinger(transport* api, int dst, double timeout) :
  my_api_(api),
  dst_(dst),
  timeout_(timeout),
  //this is necessary, I haven't figured out why
  //for some reason the start time isn't getting configured properly for some pings
  //this causes the renew pings check to fail if the start time random bits are a big number
  start_time_(-1),
  failed_(false),
  arrived_(true)
{
}

pinger::~pinger()
{
}

void
pinger::arrived()
{
  debug_printf(sumi_ping,
    "Rank %d received ping %p from neighbor %d ",
     my_api_->rank(), this, dst_);
  arrived_ = true;
}

void
pinger::attach_listener(sumi::timeout_function* func)
{
  functions_.append(func);

  debug_printf(sumi_ping,
    "Rank %d attaching function %p to ping %p to neighbor %d ",
    my_api_->rank(), func, this, dst_);
}

void
pinger::timeout_all_listeners()
{
  debug_printf(sumi_ping,
   "Rank %d %p timed out pinging %d, arrived? %d ",
    my_api_->rank(), this, dst_, arrived_);

  functions_.timeout_all_listeners(dst_);
}



void
pinger::execute()
{
  if (refcount() == 0){
  debug_printf(sumi_ping,
     "Rank %d skipping ping %p to neighbor %d for refcount=%d arrived=%d",
     my_api_->rank(), this, dst_, functions_.refcount(), arrived_);
    return;
  }
  
  debug_printf(sumi_ping,
     "Rank %d executing ping %p to neighbor %d for refcount=%d ",
     my_api_->rank(), this, dst_, functions_.refcount());

  if (arrived_){
    schedule_next();
  }
  else {
    failed_ = true;
    my_api_->declare_failed(dst_);
    //while (!functions_.empty()){
      debug_printf(sumi_ping,
        "Rank %d timing out ping %p to neighbor %d for refcount=%d ",
        my_api_->rank(), this, dst_, functions_.refcount());
      timeout_all_listeners();
    //}
  }
}

void
pinger::cancel(timeout_function* func)
{
  int refcount = functions_.erase(func);

  debug_printf(sumi_ping,
    "Rank %d canceling ping %p to neighbor %d for function %p, refcount=%d, arrived? %d ",
    my_api_->rank(), this, dst_, func, refcount, arrived_);
}

void
pinger::maybe_renew(double wtime)
{
  //debug_printf(sumi_ping,
  //  "Rank %d checking if ping to %d that started at %8.4e with timeout %8.4e should be renewed at %8.4e",
  //  my_api_->rank(), dst_, start_time_, timeout_, wtime);

  if (arrived_ && ((start_time_ + timeout_) < wtime)){
    //enough time has elapsed since the last ping - go for it
    schedule_next(wtime);
  }
}

void
pinger::schedule_next(double wtime)
{
  debug_printf(sumi_ping,
    "Rank %d %p pinging neighbor %d with timeout %8.4e s",
    my_api_->rank(), this, dst_, timeout_);

  arrived_ = false;
  start_time_ = wtime;
  my_api_->send_ping_request(dst_);

  wait();
}

void
pinger::schedule_next()
{
  schedule_next(my_api_->wall_time());
}

void
pinger::wait()
{
  arrived_ = false;
  my_api_->schedule_ping_timeout(this, timeout_);
}

void
pinger::start()
{
  arrived_ = false;
  schedule_next();
}


ping_monitor::ping_monitor(sprockit::sim_parameters* params, transport* tport) :
  activity_monitor(params, tport)
{
  timeout_ = params->get_optional_time_param("ping_timeout", 1e-3);
}

void
ping_monitor::renew_pings(double wtime)
{
  std::map<int, pinger*>::iterator it, end = pingers_.end();
  for (it=pingers_.begin(); it != end; ++it){
    pinger* p = it->second;
    p->maybe_renew(wtime);
  }
}

void
ping_monitor::validate_all_pings()
{
  double wtime = api_->wall_time();
  std::map<int, pinger*>::iterator it, end = pingers_.end();
  for (it=pingers_.begin(); it != end; ++it){
    pinger* p = it->second;
    if (p->is_expired(wtime)){
      debug_printf(sumi_ping,
        "Rank %d expiring ping %p at t=%8.4e after too long delay from t=%8.4e",
        api_->rank(), p, wtime, p->start_time());
      p->execute();
    }
  }
}

void
ping_monitor::message_received(const message::ptr& msg)
{
#if SST_SANITY_CHECK
  if (msg->class_type() != message::ping){
    spkt_throw(sprockit::illformed_error,
        "ping monitor received non-ping message");
  }
#endif
  //this is a bit weird again
  //even though I sent a ping to a partner, I sent an RDMA get
  //thus, technically speaking, I receive data from the partner
  //so I am the destination and my ping partner is the source of the data

  int ping_partner = msg->sender();
  pinger* my_ping = pingers_[ping_partner]; //who I was pinging
  if (!my_ping){
    //this might be a timed-out ping... so this just happens to arrive too late

    // this used to be a sanity check, but forget it
    spkt_throw_printf(sprockit::illformed_error,
        "sumi_api::handle: rank %d got ping back from %d, but I am not pinging it",
        api_->rank(), ping_partner);
    return;
  }

  if (msg->payload_type() == message::rdma_get_nack){
   debug_printf(sumi_ping,
    "Rank %d got ping nack from %d",
    api_->rank(), ping_partner);
    // oh snap, he's dooooowwwwnnnn
    // call the timeout a little early
    my_ping->execute();
    pingers_.erase(ping_partner);
  }
  else {
    debug_printf(sumi_ping,
        "Rank %d, returned successful ping from %d : %s",
        api_->rank(), ping_partner,
        (my_ping->refcount() ? "refcount pending, not erasing" : "refcount 0, erasing ping"));
    my_ping->arrived();
    if (my_ping->refcount() == 0){
      pingers_.erase(ping_partner);
    }
  }
}

void
ping_monitor::validate_done()
{
  debug_printf(sumi_ping,
    "Rank %d validating pings are done: %d pings pending",
    api_->rank(), pingers_.size());
  std::map<int, pinger*>::iterator it, end = pingers_.end();
  for (it=pingers_.begin(); it != end; ++it){
    pinger* p = it->second;
    int dst = it->first;
    if (p->refcount() != 0){
       spkt_throw_printf(sprockit::illformed_error,
        "rank %d still has pending ping to %d with refcount %d",
        api_->rank(), dst, p->refcount());
    }
  }
}

void
ping_monitor::ping(int dst, timeout_function* func)
{
  pinger*& my_ping = pingers_[dst];
  if (my_ping == 0){
    debug_printf(sumi_ping,
        "Rank %d totally new ping to neighbor %d for function %p",
        api_->rank(), dst, func);
    my_ping = new pinger(api_, dst, timeout_); //just do a ms for now
    my_ping->start();
  }
  else if (my_ping->refcount() == 0){ //nobody is waiting on this anymore, but still waiting for last ping
    if (my_ping->has_arrived()){
      spkt_throw_printf(sprockit::illformed_error,
        "sumi_api::ping: Rank %d canceled ping to %d is arrived, but not deleted",
        api_->rank(), dst);
    }
    debug_printf(sumi_ping,
        "Rank %d waiting anew on ping to neighbor %d for function %p",
        api_->rank(), dst, func);
    my_ping = new pinger(api_, dst, timeout_); //just do a ms for now
    my_ping->wait();
  }
  else if (my_ping->has_failed()){
    // the only way this happens is if our ping is started by the ping's failure
    //no need to ping this! - attach and be failed
    debug_printf(sumi_ping,
        "Rank %d attaching to failed ping %p to neighbor %d for function %p refcount=%d",
        api_->rank(), my_ping, dst, func, my_ping->refcount());
  }
  else {
    debug_printf(sumi_ping,
        "Rank %d attaching to existing ping %p to neighbor %d for function %p refcount=%d",
        api_->rank(), my_ping, dst, func, my_ping->refcount());
  }
  my_ping->attach_listener(func);
}

void
ping_monitor::cancel_ping(int dst, timeout_function* func)
{
  std::map<int, pinger*>::iterator it = pingers_.find(dst);
  if (it == pingers_.end()){
    spkt_throw_printf(sprockit::value_error,
      "sumi_api::cancel_tag: no pinger known for neighbor %d",
       dst);
  }
  pinger* my_ping = it->second;
  my_ping->cancel(func);
  bool ping_recved = my_ping->has_failed() || my_ping->has_arrived();
  bool ping_clear = my_ping->refcount() == 0;
  if (ping_recved && ping_clear){
    debug_printf(sumi_ping,
        "Rank %d erasing ping to neighbor %d for function %p ",
        api_->rank(), dst, func);
    pingers_.erase(dst);
  }
  else {
    debug_printf(sumi_ping,
       "Rank %d not erasing ping to neighbor %d for function %p with refcount %d: arrived? %d failed %d ",
       api_->rank(), dst, func, my_ping->refcount(), my_ping->has_arrived(), my_ping->has_failed());
  }
}

}