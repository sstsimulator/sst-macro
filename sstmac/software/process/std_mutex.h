#ifndef sstmac_sw_process_std_mutex_h
#define sstmac_sw_process_std_mutex_h

#include <sstmac/software/process/app_fwd.h>

namespace sstmac {
namespace sw {

class stdMutex {
 public:
  stdMutex();

  ~stdMutex();

  void lock();

  void unlock();

  bool try_lock();

 private:
  int id_;
  App* parent_app_;
};

class stdRecursiveMutex {
};

}
}

namespace std {
using sstmac_mutex = sstmac::sw::stdMutex;
}

#endif

