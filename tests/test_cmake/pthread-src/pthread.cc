#include <pthread.h>
void *func1(void* arg) { return 0; }
main() {
  pthread_t thread1;
  pthread_create(&thread1,NULL,func1,NULL);
  pthread_join(thread1,NULL);
  return 0;
}
