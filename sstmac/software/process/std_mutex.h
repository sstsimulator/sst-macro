#ifndef sstmac_sw_process_std_mutex_h
#define sstmac_sw_process_std_mutex_h

#include <sstmac/software/process/app_fwd.h>

namespace sstmac {
namespace sw {

class std_mutex {
 public:
  std_mutex();

  ~std_mutex();

  void lock();

  void unlock();

  bool try_lock();

 private:
  int id_;
  app* parent_app_;
};

}
}

namespace std {
using sstmac_mutex = sstmac::sw::std_mutex;
}

#endif

