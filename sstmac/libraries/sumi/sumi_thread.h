#ifndef SUMI_THREAD_H
#define SUMI_THREAD_H

#include <sstmac/software/process/app.h>

namespace sstmac {

class sumi_thread :
  public sstmac::sw::thread
{
 private:
  static uint64_t num_threads_;

  virtual void run() = 0;

 public:
  sumi_thread(sprockit::sim_parameters* params, sw::software_id sid,
              sw::operating_system* os);

  virtual ~sumi_thread(){}

  void start();

  void compute(double sec);
};

}

#endif // SUMI_THREAD_H
