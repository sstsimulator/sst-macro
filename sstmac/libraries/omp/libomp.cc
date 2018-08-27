#include <sstmac/libraries/pthread/sstmac_pthread_impl.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/app.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/replacements/omp.h>

#undef omp_init_lock
#undef omp_destroy_lock
#undef omp_set_lock
#undef omp_unset_lock
#undef omp_test_lock
#undef omp_get_num_threads
#undef omp_get_thread_num
#undef omp_get_max_threads
#undef omp_get_wtime
#undef omp_get_num_procs
#undef omp_set_num_threads
#undef omp_in_parallel
#undef omp_get_level
#undef omp_get_ancestor_thread_num

namespace sstmac {
namespace sw {

extern "C"
void sstmac_omp_init_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_init(lock, nullptr);
}

extern "C"
void sstmac_omp_destroy_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_destroy(lock);
}

extern "C"
void sstmac_omp_set_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_lock(lock);
}

extern "C"
void sstmac_omp_unset_lock(sstmac_omp_lock_t *lock)
{
  SSTMAC_pthread_mutex_unlock(lock);
}

extern "C"
int sstmac_omp_test_lock(sstmac_omp_lock_t *lock)
{
  return SSTMAC_pthread_mutex_trylock(lock);
}

extern "C"
double sstmac_omp_get_wtime(){
  return sstmac::sw::operating_system::current_os()->now().sec();
}

extern "C"
int sstmac_omp_get_thread_num(){
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  return t->omp_get_thread_num();
}

extern "C"
int sstmac_omp_get_num_threads(){
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  return t->omp_get_num_threads();
}

extern "C"
int sstmac_omp_get_max_threads(){
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  //for now, just return number of cores
  return t->omp_get_max_threads();
}

extern "C"
int sstmac_omp_get_num_procs()
{
  sstmac::sw::operating_system* os = sstmac::sw::operating_system::current_os();
  return os->node()->proc()->ncores();
}

extern "C"
void sstmac_omp_set_num_threads(int nthr)
{
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  t->omp_set_num_threads(nthr);
}

extern "C"
int sstmac_omp_in_parallel()
{
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  return t->omp_in_parallel();
}

extern "C"
int sstmac_omp_get_level()
{
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  return t->omp_get_level();
}

extern "C"
int sstmac_omp_get_ancestor_thread_num()
{
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  return t->omp_get_ancestor_thread_num();
}

}
}
