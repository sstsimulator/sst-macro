/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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
#ifndef sstmac_sw_process_ipc_tunnel_h
#define sstmac_sw_process_ipc_tunnel_h

#include <atomic>
#include <cstring>
#include <errno.h>
#include <fcntl.h>
#include <sprockit/errors.h>
#include <string>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

namespace sstmac {
namespace sw {

template <class T> class IPCTunnel {

public:
  IPCTunnel(std::string name, bool isShadow)
      : name_(std::move(name)), size_(getMapSize()) {
    if (isShadow) {
      t_ = new (initShadowRegion()) T;
    } else {
      t_ = (T *)initPuppetRegion();
    }
  }

  ~IPCTunnel() {
    munmap(t_, size_);
    close(fd_);
  }

  T *get() const { return t_; }

private:
  std::string name_;
  T *t_ = nullptr;
  int size_;
  int fd_ = 0;

  int getMapSize() {
    const auto page_size = sysconf(_SC_PAGESIZE);
    const int num_pages =
        sizeof(T) / page_size + 1; // add an extra for good measure
    return num_pages * page_size;
  }

  void *mmapSharedRegion() {
    void *ptr = mmap(NULL, size_, PROT_READ | PROT_WRITE, MAP_SHARED, fd_, 0);
    if (ptr == MAP_FAILED) {
      shm_unlink(name_.c_str());
      spkt_abort_printf(
          "create mmap region of size %d on fd %d for region %s: %s", size_,
          fd_, name_.c_str(), ::strerror(errno));
    }
    return ptr;
  }

  int getPuppetFileDescriptor() {
    int fd = shm_open(name_.c_str(), O_RDWR, S_IRUSR | S_IWUSR);
    if (fd_ == -1) {
      spkt_abort_printf("failed attaching shm region %s", name_.c_str());
    }

    return fd;
  }

  int getShadowFileDescriptor() {
    int fd =
        shm_open(name_.c_str(), O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);

    if (fd == -1) {
      spkt_abort_printf("failed creating shm region %s", name_.c_str());
    } else if (ftruncate(fd, size_)) {
      spkt_abort_printf("failed truncating fd %d for region %s to size %d", fd,
                        name_.c_str(), size_);
    }

    return fd;
  }

  void *initPuppetRegion() {
    fd_ = getPuppetFileDescriptor();
    return mmapSharedRegion();
  }

  void *initShadowRegion() {
    fd_ = getShadowFileDescriptor();
    return mmapSharedRegion();
  }
};

class ShadowPuppetSync {
private:
  char ArielTunnelString[256];
  std::atomic<std::int32_t> progressFlag;
  std::atomic<bool> NameSet;

public:
  ShadowPuppetSync() : progressFlag(0), NameSet(false) {}
  ShadowPuppetSync(ShadowPuppetSync const &) = delete;
  ShadowPuppetSync &operator=(ShadowPuppetSync const &) = delete;

  bool allowPuppetEnter() const { return progressFlag.load(); }

  bool allowShadowExit() const { return !progressFlag.load(); }

  void setAllowShadowExit() { progressFlag.store(false); }

  void setAllowPuppetEnter() { progressFlag.store(true); }

  void setTunnelName(const std::string &name) {
    std::strcpy(ArielTunnelString, name.c_str());
    NameSet.store(true); // CST should ensure that copy is complete
  }

  char const *getTunnelName() const {
    while (!NameSet.load()) {
    }
    return ArielTunnelString;
  }
};

} // namespace sw
} // namespace sstmac
#endif
