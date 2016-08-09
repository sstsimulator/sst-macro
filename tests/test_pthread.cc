#include <sprockit/test/test.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/libraries/pthread/sstmac_pthread.h>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>

using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;

extern "C" int ubuntu_cant_name_mangle() { return 0; }

void* ptest(void* args)
{
   SSTMAC_compute(1); 
   printf("Yes, I reach here!\n");
   return 0;
}

void* ptest3(void* args)
{
   SSTMAC_compute(1);
   printf("No, I do not reach here!\n");
   return 0;
}

struct pthread_args
{
  pthread_cond_t cond;
  pthread_mutex_t mutex;
};

void* ptest2(void* args)
{
  int status;
  pthread_args* pargs = (pthread_args*) args;
  SSTMAC_compute(0.001);
  status = pthread_mutex_lock(&pargs->mutex);
  if (status != 0){
    spkt_throw(sprockit::illformed_error,
        "mutex failed lock");
  }
  std::cout << "Mutex locked" << std::endl;
  SSTMAC_compute(0.001);
  std::cout << "Mutex unlocked" << std::endl;
  pthread_mutex_unlock(&pargs->mutex);
  status = pthread_mutex_lock(&pargs->mutex);
  if (status != 0){
    spkt_throw(sprockit::illformed_error,
        "mutex failed lock");
  }
  std::cout << "Condition locked" << std::endl;
  SSTMAC_compute(0.001);
  status = pthread_cond_wait(&pargs->cond, &pargs->mutex);
  if (status != 0){
    spkt_throw(sprockit::illformed_error,
        "thread failed wait");
  }
  std::cout << "Done waiting" << std::endl;
  SSTMAC_compute(0.001);

  return 0;
}

#define sstmac_app_name test_pthread


int USER_MAIN(int argc, char** argv)
{
    void* no_args = 0;
    pthread_t thr1, thr2;
    pthread_attr_t attr1, attr2;
    int status;
    status = pthread_create(&thr1, &attr1, &ptest, no_args);
    status = pthread_create(&thr2, &attr2, &ptest, no_args);

    void* ret;
    pthread_join(thr1, &ret);
    pthread_join(thr2, &ret);

    pthread_args pargs;
    pthread_mutex_init(&pargs.mutex,0);
    pthread_cond_init(&pargs.cond,0);
    status = pthread_create(&thr1, &attr1, &ptest2, &pargs);
    status = pthread_create(&thr2, &attr2, &ptest2, &pargs);

    std::cout << "Spawned threads" << std::endl;
    SSTMAC_compute(1);

    std::cout << "First signal" << std::endl;
    pthread_cond_signal(&pargs.cond);
    std::cout << "Second signal" << std::endl;
    pthread_cond_signal(&pargs.cond);

    pthread_join(thr1, &ret);
    pthread_join(thr2, &ret);

    //spin off another pthread
    status = pthread_create(&thr1, &attr1, &ptest, &pargs);

    return 0;
}





