
#include "lock_array.h"

uint32_t
hash(int index)
{
  //jenkins hash the integer
  char* key = reinterpret_cast<char*>(&index);
  const int length = sizeof(uint32_t);
  uint32_t hash = 0;
  for(int i = 0; i < length; ++i)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

void
lock_array::init(int nlocks)
{
  num_locks_ = nlocks;
  locks_.resize(nlocks);
  for (int i=0; i < nlocks; ++i){
    locks_[i] = sstmac::sim_thread_lock::construct();
	}
}

void
lock_array::lock(int index)
{
  uint32_t idx = hash(index) % num_locks_;
  locks_[idx]->lock();
}

void
lock_array::unlock(int index)
{
  uint32_t idx = hash(index) % num_locks_;
  locks_[idx]->unlock();
}

void
lock_array::print_scatter(int nIndex)
{
  std::vector<int> counts(num_locks_);
  for (int i=0; i < nIndex; ++i){
    int idx = hash(i) % num_locks_;
    printf("A[%d]=%d\n", i, idx);
    counts[idx]++;
  }

  for (int i=0; i < num_locks_; ++i){
    printf("Total[%d]=%d\n", i, counts[i]);
  }

}

