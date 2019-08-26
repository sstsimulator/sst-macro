#ifndef sstmac_sw_process_ipc_tunnel_h
#define sstmac_sw_process_ipc_tunnel_h

#include <atomic>
#include <string>
#include <fcntl.h>
#include <errno.h>
#include <cstring>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

template <class T>
class IPCTunnel {

 public:
  IPCTunnel(const std::string& name, bool create) :
    name_(name)
  {
    auto page_size = sysconf(_SC_PAGESIZE);
    int num_pages = sizeof(T) / page_size + 1; //add an extra for good measure
    size_ = num_pages * page_size;
    if (create){
      fd_ = shm_open(name.c_str(), O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
      if (fd_ == -1){
        spkt_abort_printf("failed creating shm region %s", name.c_str());
      }
      if (ftruncate(fd_, size_)){
        spkt_abort_printf("failed truncating fd %d for region %s to size %d",
                          fd_, name.c_str(), size_);
      }
      void* ptr = mmap(NULL, size_, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);
      if (ptr == MAP_FAILED){
        shm_unlink(name.c_str());
        spkt_abort_printf("create mmap region of size %d on fd %d for region %s: %s",
                          size_, fd_, name.c_str(), ::strerror(errno));
      }
      t_ = new (ptr) T;
    } else {
      fd_ = shm_open(name.c_str(), O_RDWR, S_IRUSR|S_IWUSR);
      if (fd_ == -1){
        spkt_abort_printf("failed attaching shm region %s", name.c_str());
      }
      void* ptr = mmap(NULL, size_, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);
      if (ptr == MAP_FAILED){
        shm_unlink(name.c_str());
        spkt_abort_printf("attach mmap region of size %d on fd %d for region %s: %s",
                          size_, fd_, name.c_str(), ::strerror(errno));
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
  int size_;
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

