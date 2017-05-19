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

#ifndef SERIAL_RUNTIME_H
#define SERIAL_RUNTIME_H

#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {
namespace native {

class serial_runtime :
  public parallel_runtime
{
  FactoryRegister("serial", parallel_runtime, serial_runtime)
 public:
  serial_runtime(sprockit::sim_parameters* params);

  int64_t allreduce_min(int64_t mintime) override;

  int64_t allreduce_max(int64_t maxtime) override;

  void global_sum(long *data, int nelems, int root) override;

  void global_sum(long long *data, int nelems, int root) override;

  void global_max(int *data, int nelems, int root) override;

  void global_max(long *data, int nelems, int root) override;

  void gather(void *send_buffer, int num_bytes, void *recv_buffer, int root) override;

  void allgather(void *send_buffer, int num_bytes, void *recv_buffer) override;

  void send(int dst, void *buffer, int buffer_size) override;

  void recv(int src, void *buffer, int buffer_size) override;

  void bcast(void* buffer, int bytes, int root) override;

  void finalize() override {}

  /**
   * @param The topology id to send a remote message to
   * @param buffer The buffer containing a serialized message
   * @param size The size of the buffer being sent
   */
  void send_event(timestamp t, switch_id sid, event* ev);

  void wait_merge_array(int tag) override;

  void declare_merge_array(void* buffer, int size, int tag) override;

  bool release_merge_array(int tag) override;

 protected:
  void do_send_message(int lp, void* buffer, int size) override;

  void do_send_recv_messages(std::vector<void*>& buffers) override;

  std::map<int, int> merge_refcounts_;

};


}
}



#endif // SERIAL_RUNTIME_H