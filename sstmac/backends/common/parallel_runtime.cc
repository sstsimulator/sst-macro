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

#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/serializable.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/output.h>
#include <sprockit/fileio.h>
#include <fstream>
#include <sstream>
#include <sprockit/keyword_registration.h>
#include <sprockit/thread_safe.h>
#include <sprockit/printable.h>

RegisterDebugSlot(parallel);

RegisterKeywords(
{ "serialization_buffer_size", "the size of the default serialization buffer for pairwise sends" },
{ "backup_buffer_size", "the size of the backup buffer in case main buffer overflows" },
{ "partition", "the partitioning algorithm for assigning work to logical processes" },
{ "runtime", "the underlying runtime (usually MPI or serial) managing logical processes" },
{ "sst_nthread", "the number of threads to use" },
{ "sst_nproc", "the number of parallel procs (ranks) to use" }
);

namespace sstmac {

const int parallel_runtime::global_root = -1;
parallel_runtime* parallel_runtime::static_runtime_ = nullptr;
static int backupSize = 10e6;

char*
parallel_runtime::comm_buffer::allocateSpace(size_t size, ipc_event_t *ev)
{
  uint64_t newOffset = add_int64_atomic(size, &bytesAllocated);
  uint64_t myStartPos = newOffset - size;
  if (newOffset > allocSize){
    lock();
    //see if I am the first to overrun - if so, set the filled size
    if (myStartPos <= allocSize){
      filledSize = myStartPos;
    }

    //find me a backup buffer meeting my requirements
    for (backup_buffer& b : backups){
      if (newOffset < b.maxSize){
        //great - this is my backup buffer
        b.filledSize = std::max(newOffset, b.filledSize);
        unlock();
        return b.buffer + myStartPos;
      }
    }

    uint64_t nextBackupSize = backups.empty() ? backupSize : backups.back().maxSize * 8;
    while (nextBackupSize < newOffset){
      nextBackupSize *= 8;
    }

    //create a new larger backup buffer big enough to hold
    char* buf = new char[nextBackupSize];
    backup_buffer b;
    b.buffer = buf;
    b.maxSize = nextBackupSize;
    b.filledSize = newOffset;
    backups.push_back(b);
    unlock();

    return buf + myStartPos;
  } else {
    //great - good to go, write to this location
    return storage + myStartPos;
  }
}

void
parallel_runtime::comm_buffer::copyToBackup()
{
  if (backups.empty()) return;

  size_t lastFill = 0;
  size_t fillMark = filledSize;
  char* finalBuf = backups.back().buffer;
  char* nextBuf = storage;
  for (backup_buffer& buf : backups){
    size_t bytesToFill = fillMark - lastFill;
    ::memcpy(finalBuf + lastFill, nextBuf + lastFill, bytesToFill);
    lastFill += bytesToFill;
    fillMark = buf.filledSize;
    nextBuf = buf.buffer;
  }
}

void
parallel_runtime::comm_buffer::reset()
{
  if (!backups.empty()){
    int growRatio = bytesAllocated / allocSize;
    growRatio = std::max(2,growRatio);
    growRatio = std::min(growRatio, 8);
    realloc(allocSize*growRatio);
    for (auto& buf : backups){
      delete[] buf.buffer;
    }
    backups.clear();
  }
  filledSize = 0;
  bytesAllocated = 0;
}

void
parallel_runtime::comm_buffer::realloc(size_t size)
{
  char* oldAlloc = allocation;
  allocSize = size;
  allocation = new char[allocSize+64];
  storage = allocation;
  align64(storage);
  bytesAllocated = 0;
  if (oldAlloc) delete[] oldAlloc;
}

void
parallel_runtime::bcast_string(std::string& str, int root)
{
  if (nproc_ == 1)
    return;

  if (root == me_){
    int size = str.size(); //+1 for null terminator
    int root = 0;
    bcast(&size, sizeof(int), root);
    char* buf = const_cast<char*>(str.c_str());
    bcast(buf, size, root);
  } else {
    int size;
    bcast(&size, sizeof(int), root);
    str.resize(size);
    char* buf = const_cast<char*>(str.c_str());
    bcast(buf, size, root);
  }
}

std::istream*
parallel_runtime::bcast_file_stream(const std::string &fname)
{

  if (me_ == 0){
    std::ifstream* fstr = new std::ifstream;
    sprockit::SpktFileIO::open_file(*fstr, fname);

    if (!fstr->is_open()) {
      spkt_throw_printf(sprockit::input_error,
       "could not find file %s in current folder or configuration include path",
       fname.c_str());
    }

    if (nproc_ == 1){
      return fstr; //nothing to do
    }

    std::stringstream sstr;
    std::string line;
    while (fstr->good()){
      std::getline(*fstr, line);
      sstr << line << "\n";
    }
    std::string all_text = sstr.str();
    bcast_string(all_text, 0);
    //go back to the beginning of the file
    fstr->clear();
    fstr->seekg(0, fstr->beg);
    return fstr;
  } else {
    std::string all_text;
    bcast_string(all_text, 0);
    return new std::stringstream(all_text);
  }
}

void
parallel_runtime::init_partition_params(sprockit::sim_parameters *params)
{
#if SSTMAC_INTEGRATED_SST_CORE
  sprockit::abort("parallel_runtime::init_partition_params: should not be used with integrated core");
#else
  //out with the old, in with the new
  if (part_) delete part_;
  std::string deflt = "block";
  if (nthread_ == 1 && nproc_ == 1){
    deflt = "serial";
  }
  part_ = partition::factory::get_optional_param("partition", deflt, params, this);
#endif
}

parallel_runtime*
parallel_runtime::static_runtime(sprockit::sim_parameters* params)
{
#if SSTMAC_INTEGRATED_SST_CORE
  return nullptr;
#else
  static thread_lock rt_lock;
  rt_lock.lock();
  if (!static_runtime_){
    static_runtime_ = parallel_runtime::factory::get_param("runtime", params);
  }
  rt_lock.unlock();
  return static_runtime_;
#endif
}

void
parallel_runtime::init_runtime_params(sprockit::sim_parameters *params)
{
  num_recvs_done_ = 0;
  num_sends_done_ = 0;
  sends_done_.resize(nproc_);

  //turn the number of procs and my rank into keywords
  nthread_ = params->get_optional_int_param("sst_nthread", 1);

  buf_size_ = params->get_optional_byte_length_param("serialization_buffer_size", 16384);

  backupSize = params->get_optional_byte_length_param("backup_buffer_size", 10e6);

  send_buffers_.resize(nproc_);
  recv_buffers_.resize(nproc_);
  for (int i=0; i < nproc_; ++i){
    send_buffers_[i].realloc(buf_size_);
    recv_buffers_[i].realloc(buf_size_);
  }

#if !SSTMAC_USE_MULTITHREAD
  if (nthread_ > 1){
    spkt_abort_printf("must compile with --enable-multithread to run with >1 thread");
  }
#endif
}

parallel_runtime::parallel_runtime(sprockit::sim_parameters* params,
                                   int me, int nproc)
  : part_(nullptr),
    me_(me),
    nproc_(nproc),
    nthread_(1)
{
  if (me_ == 0){
    sprockit::output::init_out0(&std::cout);
    sprockit::output::init_err0(&std::cerr);
  }
  else {
    sprockit::output::init_out0(new std::ofstream("/dev/null"));
    sprockit::output::init_err0(new std::ofstream("/dev/null"));
  }
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);
}

parallel_runtime::~parallel_runtime()
{
  if (part_) delete part_;
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
parallel_runtime::run_serialize(serializer& ser, ipc_event_t* iev)
{
  ser & iev->ser_size; //this must be first!!!
  ser & iev->dst;  //this must be first!!!
  ser & iev->t;
  ser & iev->src;
  ser & iev->seqnum;
  ser & iev->port;
  ser & iev->rank;
  ser & iev->credit;
  ser & iev->ev;
}

void parallel_runtime::send_event(ipc_event_t* iev)
{
  //somehow this doesn't return the sum of sizes
  //uint32_t overhead = sizeof(ipc_event_base);
  const uint32_t overhead = sizeof(uint32_t) + sizeof(uint32_t)
    + sizeof(timestamp) + sizeof(uint32_t) + sizeof(uint32_t)
    + sizeof(int) + sizeof(int) + sizeof(bool);

  sprockit::serializer ser;
  ser.start_sizing();
  ser & iev->ev;
  iev->ser_size = overhead + ser.size();
  align64(iev->ser_size);
  comm_buffer& buff = send_buffers_[iev->rank];
  char* ptr = buff.allocateSpace(iev->ser_size, iev);
  ser.start_packing(ptr, iev->ser_size);
  debug_printf(sprockit::dbg::parallel, "sending event of size %lu to LP %d at t=%10.6e: %s",
               iev->ser_size, iev->rank, iev->t.sec(),
               sprockit::to_string(iev->ev).c_str());
  run_serialize(ser, iev);
}
#endif

void
parallel_runtime::reset_send_recv()
{
  for (int i=0; i < num_sends_done_; ++i){
    send_buffers_[sends_done_[i]].reset();
  }
  for (int i=0; i < num_recvs_done_; ++i){
    recv_buffers_[i].reset();
  }
  num_sends_done_ = 0;
  num_recvs_done_ = 0;
}


}
