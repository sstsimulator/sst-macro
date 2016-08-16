#include <sched.h>
#include <pthread.h>

void*
pthread_run(void* args){
  int thr = *((int*)args);
  sstmac_compute(1);
  printf("Finishing compute at T=%8.4f on thread %d\n",
         sstmac_now(), thr);
  return 0;
}

#define sstmac_app_name user_app_cxx

int main(int argc, char** argv)
{
  int nthread = 10;
  pthread_attr_t* attrs = new pthread_attr_t[nthread];
  for (int i=0; i < nthread; ++i){
    pthread_attr_init(&attrs[i]);
  }
  
  pthread_t* threads = new pthread_t[nthread];
  int num_on_core = 4;
  int thread_id = 0;
  int core = 0;

  //assign 4->0, 3-1, 2->2, 1->3 for a total of ten threads
  while (thread_id < nthread){
    for (int i=0; i < num_on_core; ++i, ++thread_id){
      cpu_set_t set;
      CPU_ZERO(&set);
      CPU_SET(core, &set);
      pthread_attr_setaffinity_np(&attrs[thread_id], sizeof(cpu_set_t),  &set);
    }
    ++core;
    --num_on_core;
  }
  
  void* args = 0;
  int* thread_ids = new int[nthread];
  for (int i=0; i < nthread; ++i){
    thread_ids[i] = i;
    pthread_create(&threads[i], &attrs[i], pthread_run, &thread_ids[i]);
  }
  for (int i=0; i < nthread; ++i){
    pthread_join(threads[i], &args);
  }

  return 0;
}
