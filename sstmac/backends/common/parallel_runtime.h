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

#ifndef PARALLEL_RUNTIME_H
#define PARALLEL_RUNTIME_H


#include <sstmac/common/messages/message_buffer_cache.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_location.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/backends/common/sim_partition_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters.h>
#include <list>

DeclareDebugSlot(parallel);

namespace sstmac {

class parallel_runtime :
  public lockable
{
  DeclareFactory(parallel_runtime)
 public:
  virtual ~parallel_runtime();

  struct comm_buffer {
    char* ptr;
    size_t remaining;
    size_t allocSize;
    char* storage;
    std::list<char*> expiredArrays;

    comm_buffer() : storage(nullptr), ptr(nullptr) {}

    void init(size_t size){
      allocSize = size;
      storage = new char[allocSize];
      ptr = storage;
      remaining = allocSize;
    }

    void resize(size_t size){
      if (size <= allocSize) return;
      expiredArrays.push_back(storage);
      init(size);
    }

    void reset(){
      for (char* old : expiredArrays){
        delete[] old;
      }
      expiredArrays.clear();
      ptr = storage;
      remaining = allocSize;
    }

    void grow(){
      if (!ptr) spkt_abort_printf("comm_buffer cannot grow - no previous storage allocated");
      expiredArrays.push_back(storage);
      init(allocSize*2);
    }

    void shift(size_t size){
      ptr += size;
      remaining -= size;
    }

  };

  static const int global_root;

  virtual int64_t allreduce_min(int64_t mintime) = 0;

  virtual int64_t allreduce_max(int64_t maxtime) = 0;

  virtual void global_sum(long* data, int nelems, int root) = 0;

  virtual void global_sum(long long* data, int nelems, int root) = 0;

  virtual void global_max(int* data, int nelems, int root) = 0;

  virtual void global_max(long* data, int nelems, int root) = 0;

  virtual void send(int dst, void* buffer, int buffer_size) = 0;

  virtual void gather(void* send_buffer, int num_bytes, void* recv_buffer, int root) = 0;

  virtual void allgather(void* send_buffer, int num_bytes, void* recv_buffer) = 0;

  virtual void recv(int src, void* buffer, int buffer_size) = 0;

  int global_max(int my_elem){
    int dummy = my_elem;
    global_max(&dummy, 1, global_root);
    return dummy;
  }

  long global_max(long my_elem){
    long dummy = my_elem;
    global_max(&dummy, 1, global_root);
    return dummy;
  }

  virtual void bcast(void* buffer, int bytes, int root) = 0;

  void bcast_string(std::string& str, int root);

  std::istream* bcast_file_stream(const std::string& fname);

  virtual void finalize() = 0;

  virtual void init_runtime_params(sprockit::sim_parameters* params);

  virtual void init_partition_params(sprockit::sim_parameters* params);

  /**
   @param pool A buffer cache corresponding to a pool of free buffers
   @param incoming A buffer cache holding buffers that correspond to incoming messages
  */
  virtual void send_recv_messages(std::vector<void*>& incoming);

  /**
   * @param The topology id to send a remote message to
   * @param buffer The buffer containing a serialized message
   * @param size The size of the buffer being sent
   */
  virtual void send_event(int thread_id,
    timestamp t,
    device_id tid,
    device_id src,
    uint32_t seqnum,
    event* ev);

  int me() const {
    return me_;
  }

  int nproc() const {
    return nproc_;
  }

  int nthread() const {
    return nthread_;
  }

  int ser_buf_size() const {
    return buf_size_;
  }

  partition* topology_partition() const {
    return part_;
  }

  virtual void wait_merge_array(int tag) = 0;

  virtual void declare_merge_array(void* buffer, int size, int tag) = 0;

  virtual bool release_merge_array(int tag) = 0;

  static parallel_runtime* static_runtime(sprockit::sim_parameters* params);

  static void clear_static_runtime(){
    if (static_runtime_) delete static_runtime_;
    static_runtime_ = nullptr;
  }

 protected:
  parallel_runtime(sprockit::sim_parameters* params,
                   int me, int nproc);

  virtual void
  do_send_message(int lp, void* buffer, int size) = 0;

  virtual void
  do_send_recv_messages(std::vector<void*>& buffers) = 0;

 protected:
   int nproc_;
   int nthread_;
   int me_;
   std::vector<comm_buffer> send_buffers_;
   comm_buffer recv_buffer_;
   int buf_size_;
   partition* part_;
   static parallel_runtime* static_runtime_;

};

}

#endif // PARALLEL_RUNTIME_H