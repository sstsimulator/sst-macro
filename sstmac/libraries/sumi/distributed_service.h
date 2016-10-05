#ifndef distributed_service_h
#define distributed_service_h

#include <sstmac/software/process/app.h>
#include <sstmac/software/libraries/library.h>
#include <sumi/transport_fwd.h>
#include <sumi/message_fwd.h>

namespace sstmac
{

class distributed_service :
  public sstmac::sw::app
{
 public:
  distributed_service(sprockit::sim_parameters* params, sw::software_id sid) :
    app(params, sid)
  {
  }

  void skeleton_main();

  sumi::message_ptr busy_loop(sumi::transport* tport);

 private:
  virtual void run(sumi::transport* tport) = 0;
};

}


#endif

