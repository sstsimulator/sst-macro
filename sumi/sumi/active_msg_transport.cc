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

#include <sumi/active_msg_transport.h>
#include <sys/time.h>

namespace sumi {

active_msg_transport::active_msg_transport() :
 time_zero_(0),
 smsg_buffer_(0),
 smsg_buffer_size_(512)
{
}

message::ptr
active_msg_transport::block_until_message()
{
  while (completion_queue_.empty()){
    block_inner_loop();
  }
  bool empty;
  message::ptr ret = completion_queue_.pop_front_and_return(empty);
  return ret;
}

message::ptr
active_msg_transport::block_until_message(double timeout)
{
  double start = wall_time();
  double stop = start + timeout;
  while (completion_queue_.empty() && wall_time() < stop){
    block_inner_loop();
  }
  bool empty;
  message::ptr ret = completion_queue_.pop_front_and_return(empty);
  return ret;
}

collective_done_message::ptr
active_msg_transport::collective_block(collective::type_t ty, int tag)
{
  spkt_throw(sprockit::unimplemented_error,
    "active_msg_transpot::collective_block");
}

char*
active_msg_transport::allocate_message_buffer(const message::ptr &msg, int& size)
{
  lock();
  char* ser_buffer = allocate_smsg_buffer(); fflush(stdout);
  unlock();
  sumi::serializer ser;
  ser.start_packing(ser_buffer, smsg_buffer_size_);
  ser & msg;
  size = ser.packer().size();
  return ser_buffer;
}

message::ptr
active_msg_transport::deserialize(char* ser_buffer)
{
  sumi::serializer ser;
  ser.start_unpacking(ser_buffer, smsg_buffer_size_);
  message::ptr msg;
  ser & msg;
  return msg;
}

message::ptr
active_msg_transport::free_message_buffer(void* buf)
{
  message::ptr msg = deserialize((char*)buf);
  lock();
  free_smsg_buffer(buf);
  unlock();
  return msg;
}

void
active_msg_transport::free_smsg_buffer(void* buf)
{
  smsg_buffer_pool_.push_back((char*)buf);
}

char*
active_msg_transport::allocate_smsg_buffer()
{
  if (smsg_buffer_pool_.empty()){
    spkt_throw(sprockit::value_error,
      "too many smsg buffers allocated");
  }

  char* ret = smsg_buffer_pool_.back();
  smsg_buffer_pool_.pop_back();
  return ret;
}

double
active_msg_transport::wall_time() const
{
  timeval t_st;
  gettimeofday(&t_st, 0);
  double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return t - time_zero_;
}

void
active_msg_transport::schedule_next_heartbeat()
{
  next_heartbeat_ = wall_time() + heartbeat_interval_;
}

void
active_msg_transport::maybe_do_heartbeat()
{
  if (heartbeat_active_ && !heartbeat_running_ && wall_time() > next_heartbeat_)
    next_heartbeat();
}

void
active_msg_transport::init()
{
  time_zero_ = wall_time();
  transport::init();

  static const int max_num_requests_(1000);

  smsg_buffer_ = (char*) ::malloc(smsg_buffer_size_ * max_num_requests_);
  char* smsg_ptr = smsg_buffer_;

  for (int i=0; i < max_num_requests_; ++i, smsg_ptr += smsg_buffer_size_){
    free_smsg_buffer(smsg_ptr);
  }
}

}