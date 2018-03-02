#include <sstmac/libraries/pthread/sstmac_pthread_impl.h>
#include <sstmac/replacements/omp.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/processor/processor.h>

namespace sstmac {
namespace sw {

struct omp_thread_group {
  std::vector<sstmac_pthread_t> subthreads;
};

struct omp_context {
  uint64_t cpumask;
  int max_num_threads;
  int my_id;
  omp_thread_group* group;
};

class omp_api : public api {
  RegisterAPI("omp", omp_api)
 public:
  omp_api(sprockit::sim_parameters* params,
          sstmac::sw::software_id sid,
          sstmac::sw::operating_system* os) :
    api(params, "omp", sid, os)
  {
  }

  omp_context& get_context(uint32_t tid){
    auto iter = contexts_.find(tid);
    if (iter == contexts_.end()){
      spkt_abort_printf("thread id %u is not an OpenMP thread", tid);
    }
    return iter->second;
  }

  void incoming_event(event *ev){
    //unnecessary
  }

 private:
  std::map<uint32_t,omp_context> contexts_;

};

}
}


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
  auto api = t->get_api<sstmac::sw::omp_api>();
  auto& ctx = api->get_context(t->thread_id());
  return ctx.my_id;
}

extern "C"
int sstmac_omp_get_num_threads(){
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  auto api = t->get_api<sstmac::sw::omp_api>();
  auto& ctx = api->get_context(t->thread_id());
  if (ctx.group) return ctx.group->subthreads.size();
  else return 1;
}

extern "C"
int sstmac_omp_get_max_threads(){
  sstmac::sw::thread* t = sstmac::sw::operating_system::current_thread();
  //for now, just return number of cores
  return t->os()->node()->proc()->ncores();
}


