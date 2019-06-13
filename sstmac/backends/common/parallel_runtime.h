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

#ifndef PARALLEL_RUNTIME_H
#define PARALLEL_RUNTIME_H

#include <sstmac/common/node_address.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_location.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/ipc_event.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/backends/common/sim_partition_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sprockit/factory.h>
#include <sprockit/sim_parameters.h>
#include <list>

DeclareDebugSlot(parallel);

template <class T>
void align64(T& t){
  if (t % 64){
    t = t + 64 - t%64;
  }
}

template <class T>
void align64(T*& t){
  intptr_t ptr = (intptr_t) t;
  if (ptr % 64){
    t = (T*)(ptr + 64 - ptr%64);
  }
}

namespace sstmac {

class ParallelRuntime :
  public Lockable
{
 public:
  SST_ELI_DECLARE_BASE(ParallelRuntime)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(SST::Params&)

  virtual ~ParallelRuntime();

  struct CommBuffer : public Lockable {
    int64_t bytesAllocated;
    int64_t allocSize;
    int64_t filledSize;
    char* allocation;
    char* storage;

    struct BackupBuffer {
      uint64_t maxSize;
      uint64_t filledSize;
      char* buffer;
    };

    CommBuffer() : storage(nullptr), allocation(nullptr),
      filledSize(0), bytesAllocated(0) {}

    ~CommBuffer(){
      if (allocation) delete[] allocation;
    }

    char* buffer() const {
      return storage;
    }

    char* nextBuffer() const {
      return storage + bytesAllocated;
    }

    /**
     * @brief bytesFilled
     * @return The number of bytes actually packed into the storage. Only non-zero
     *         if backup buffer is in use
     */
    size_t bytesFilled() const {
      return filledSize;
    }

    size_t totalBytes() const {
      return bytesAllocated;
    }

    bool hasBackup() const {
      return !backups.empty();
    }

    char* backup() const {
      return backups.back().buffer;
    }

    void copyToBackup();

    void reset();

    void realloc(size_t size);

    void ensureSpace(size_t size)
    {
      if (allocSize < size){
        realloc(size);
      }
    }

    void shift(size_t size){
      bytesAllocated += size;
    }

    char* allocateSpace(size_t size, IpcEvent* ev);

    std::vector<BackupBuffer> backups;

  };

#if !SSTMAC_INTEGRATED_SST_CORE
  void sendEvent(IpcEvent* iev);

  static void runSerialize(serializer& ser, IpcEvent* iev);
#endif

  static const int global_root;

  virtual int64_t allreduceMin(int64_t mintime) = 0;

  virtual int64_t allreduceMax(int64_t maxtime) = 0;

  virtual void globalSum(int32_t* data, int nelems, int root) = 0;

  virtual void globalSum(uint32_t* data, int nelems, int root) = 0;

  virtual void globalSum(int64_t* data, int nelems, int root) = 0;

  virtual void globalSum(uint64_t* data, int nelems, int root) = 0;

  virtual void globalMax(int32_t* data, int nelems, int root) = 0;

  virtual void globalMax(uint32_t* data, int nelems, int root) = 0;

  virtual void globalMax(int64_t* data, int nelems, int root) = 0;

  virtual void globalMax(uint64_t* data, int nelems, int root) = 0;

  virtual void send(int dst, void* buffer, int buffer_size) = 0;

  virtual void gather(void* send_buffer, int num_bytes, void* recv_buffer, int root) = 0;

  virtual void allgather(void* send_buffer, int num_bytes, void* recv_buffer) = 0;

  virtual void recv(int src, void* buffer, int buffer_size) = 0;

  template <class T> T globalMax(T my_elem){
    T dummy = my_elem;
    globalMax(&dummy, 1, global_root);
    return dummy;
  }

  template <class T> T globalSum(T my_elem){
    T sum = my_elem;
    globalSum(&sum, 1, global_root);
    return sum;
  }

  virtual void bcast(void* buffer, int bytes, int root) = 0;

  void bcastString(std::string& str, int root);

  std::istream* bcastFileStream(const std::string& fname);

  virtual void finalize() = 0;

  int epoch() const {
    return epoch_;
  }

  virtual void initRuntimeParams(SST::Params& params);

  virtual void initPartitionParams(SST::Params& params);

  virtual Timestamp sendRecvMessages(Timestamp vote){
    return vote;
  }

  void resetSendRecv();

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

  Partition* topologyPartition() const {
    return part_;
  }

  int numRecvsDone() const {
    return numRecvsDone_;
  }

  const CommBuffer& recvBuffer(int idx) const {
    return recv_buffers_[idx];
  }

  static ParallelRuntime* staticRuntime(SST::Params& params);

  static void clearStaticRuntime(){
    if (static_runtime_) delete static_runtime_;
    static_runtime_ = nullptr;
  }

 protected:
  ParallelRuntime(SST::Params& params,
                   int me, int nproc);

 protected:
   int nproc_;
   int nthread_;
   int me_;
   int epoch_;
   std::vector<CommBuffer> send_buffers_;
   std::vector<CommBuffer> recv_buffers_;
   std::vector<int> sends_done_;
   int num_sends_done_;
   int numRecvsDone_;
   int buf_size_;
   Partition* part_;
   static ParallelRuntime* static_runtime_;

};

}

#endif // PARALLEL_RUNTIME_H
