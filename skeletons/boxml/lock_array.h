#include <sstmac/common/sim_thread_lock.h>

class lock_array
{
 public:
  void init(int nlocks);

  void lock(int index);

  void unlock(int index);

  void print_scatter(int nIndex);

 private:
  int num_locks_;
  std::vector<sstmac::sim_thread_lock*> locks_;

};

