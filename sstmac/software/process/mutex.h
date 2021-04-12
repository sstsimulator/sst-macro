#ifndef sstmac_software_process_mutex_h
#define sstmac_software_process_mutex_h

#include <sstmac/software/process/thread_fwd.h>
#include <list>
#include <map>

namespace sstmac {
namespace sw {

class mutex_t  {
 public:
  /** Blocking keys for those threads waiting on the mutex */
  std::list<Thread*> waiters;
  std::list<Thread*> conditionals;
  bool locked;

  mutex_t() : locked(false)
  {
  }
};

typedef std::map<long, mutex_t*> condition_t;

}
}

#endif
