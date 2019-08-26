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
  IPCTunnel(std::string name, bool isShadow) :
    name_(std::move(name)), size_(getMapSize())
  {
    if (isShadow){
      t_ = new (initShadowRegion()) T;
    } else {
      t_ = (T*) initPuppetRegion();
    }
  }

  ~IPCTunnel(){
    munmap(t_, size_);
    close(fd_);
  }

  T* get() const {
    return t_;
  }

 private:
  std::string name_;
  T* t_ = nullptr;
  int size_;
  int fd_ = 0;

  int getMapSize() {
    const auto page_size = sysconf(_SC_PAGESIZE);
    const int num_pages = sizeof(T) / page_size + 1; // add an extra for good measure
    return num_pages * page_size;
  }

  void * mmapSharedRegion(){
      void* ptr = mmap(NULL, size_, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);
      if (ptr == MAP_FAILED){
        shm_unlink(name_.c_str());
        spkt_abort_printf("create mmap region of size %d on fd %d for region %s: %s",
                          size_, fd_, name_.c_str(), ::strerror(errno));
      }
      return ptr;
  }

  int getPuppetFileDescriptor() {
      int fd = shm_open(name_.c_str(), O_RDWR, S_IRUSR|S_IWUSR);
      if (fd_ == -1){
        spkt_abort_printf("failed attaching shm region %s", name_.c_str());
      }

      return fd;
  }

  int getShadowFileDescriptor() {
      int fd = shm_open(name_.c_str(), 
                        O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
      
      if (fd == -1){
        spkt_abort_printf("failed creating shm region %s", name_.c_str());
      } else if (ftruncate(fd, size_)){
        spkt_abort_printf("failed truncating fd %d for region %s to size %d",
                          fd, name_.c_str(), size_);
      }

      return fd;
  }

  void* initPuppetRegion(){ 
    fd_ = getPuppetFileDescriptor();
    return mmapSharedRegion();
  }

  void* initShadowRegion(){ 
    fd_ = getShadowFileDescriptor();
    return mmapSharedRegion();
  }

};

class ShadowPuppetSync {
 private:
  char ArielTunnelString[256];
  std::atomic_int32_t progressFlag;
  std::atomic_bool NameSet;

 public:
  ShadowPuppetSync() : progressFlag(0), NameSet(false) {}
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
    NameSet.store(true); // CST should ensure that copy is complete 
  }

  char const* getTunnelName() const {
    while(!NameSet.load()){}
    return ArielTunnelString;
  }
};

}
}
#endif

