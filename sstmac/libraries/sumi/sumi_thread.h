#ifndef SUMI_THREAD_H
#define SUMI_THREAD_H

#include <sstmac/software/process/app.h>

namespace sstmac {

class sumi_thread :
  public sstmac::sw::thread
{
 private:
  static uint64_t num_threads_;

  sstmac::sw::lib_compute_time* compute_;

  virtual void run() = 0;

 public:
  sumi_thread(sw::software_id sid);

  virtual ~sumi_thread(){}

  void start();

  void compute(double sec);
};

}

#endif // SUMI_THREAD_H
