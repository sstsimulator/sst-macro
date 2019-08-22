#ifndef sstmac_sw_process_ipc_tunnel_h
#define sstmac_sw_process_ipc_tunnel_h

namespace sstmac {
namespace sw {

#include <string>
#include <atomic>
#include <fcntl.h>
#include <errno.h>
#include <cstring>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sprockit/errors.h>

template <class T>
class IPCTunnel {

 public:
  IPCTunnel(const std::string& name, bool create) :
    name_(name)
  {
    if (create){
      fd_ = shm_open(name.c_str(), O_RDWR | O_CREAT | O_EXCL, S_IRWXU);
      if (fd_ == -1){
        spkt_abort_printf("failed creating shm region %s", name.c_str());
      }
      void* ptr = mmap(NULL, sizeof(T), PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);
      if (ptr == nullptr){
        spkt_abort_printf("failed mmap region of size %d on fd %d for region %s",
                          int(sizeof(T)), fd_, name.c_str());
      }
      t_ = new (ptr) T;
    } else {
      fd_ = shm_open(name.c_str(), O_RDWR, S_IRWXU);
      if (fd_ == -1){
        spkt_abort_printf("failed creating shm region %s", name.c_str());
      }
      void* ptr = mmap(NULL, sizeof(T), PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);
      if (ptr == nullptr){
        spkt_abort_printf("failed mmap region of size %d on fd %d for region %s",
                          int(sizeof(T)), fd_, name.c_str());
      }
      t_ = (T*) ptr;
    }
  }

  ~IPCTunnel(){
    munmap(t_, sizeof(T));
    close(fd_);
  }

  T* get() const {
    return t_;
  }

 private:
  std::string name_;
  int fd_;
  T* t_;
};


class ShadowPuppetSync {
 private:
  char ArielTunnelString[256];
  std::atomic_bool stringSet;
  std::atomic_int32_t progressFlag;

 public:
  ShadowPuppetSync() : stringSet(false), progressFlag(0) {}
  ShadowPuppetSync(ShadowPuppetSync const&) = delete;
  ShadowPuppetSync& operator=(ShadowPuppetSync const&) = delete;

  bool allowPuppetEnter() const {
    return progressFlag.load();
  }

  bool allowShadowExit() const {
    return !progressFlag.load();
  }

  void setAllowShadowExit() {
    progressFlag.store(false);
  }

  void setAllowPuppetEnter() {
    progressFlag.store(true);
  }

  void setTunnelName(const std::string& name){
    std::strcpy(ArielTunnelString, name.c_str());
    stringSet.store(true); // CST should ensure that copy is complete 
  }

  char const* getTunnelName() const {
    while(!stringSet.load()){}

    return ArielTunnelString;
  }
};

}
}
#endif

