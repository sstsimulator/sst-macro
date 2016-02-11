#ifndef sstmac_common_sim_thread_lock_h
#define sstmac_common_sim_thread_lock_h

namespace sstmac {

class sim_thread_lock
{
 public:
  static sim_thread_lock*
  construct();

  virtual void
  lock() = 0;

  virtual void
  unlock() = 0;
};

}

#endif


