#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/skeleton.h>
#include <sstmac/util.h>
#include <sumi/transport.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/libraries/sumi/distributed_service.h>

namespace sstmac {

class test_service : public distributed_service
{
 public:
  test_service(sprockit::sim_parameters* params,
               const std::string& libname,
               sw::software_id sid,
               sw::operating_system* os) :
    distributed_service(params, libname, sid, os)
  {
  }

  void run() override;
};

class service_test_message : public sumi::message
{
 public:
  typedef sprockit::refcount_ptr<service_test_message> ptr;

  std::string
  to_string() const override {
    return "service test message";
  }

  service_test_message(timestamp w) :
    sumi::message(100), workload(w)
  {
  }

  timestamp workload;
};

void
test_service::run()
{
  while (!terminated()) {
    sumi::message::ptr msg = poll_for_message(true);
    if (msg){
      service_test_message::ptr smsg = ptr_safe_cast(service_test_message, msg);
      printf("Service node %d sleeping for %8.4es\n", rank(), smsg->workload.sec());
      os_->sleep(smsg->workload);
    }
  }
}

ServiceRegister("test_service", test_service);

} //end namespace sstmac



#define sstmac_app_name test_client

using namespace sstmac;

int USER_MAIN(int argc, char** argv)
{
  sprockit::sim_parameters* params = get_params();
  int num_messages = params->get_optional_int_param("num_tasks", 10);
  timestamp task_length(1e-3);

  sumi_transport* tport = sumi::sumi_api();
  tport->init();

  sstmac::sw::app_launch* srv = sstmac::sw::app_launch::service_info("test_service");
  int num_service_nodes = srv->nproc();
  int me = tport->rank();
  int partner = me;
  for (int i=0; i < num_messages; ++i){
    partner = ((partner + 5) * 7) % num_service_nodes;
    printf("Client %d sending to service node %d at addr %d\n",
           me, partner, srv->node_assignment(partner));
    service_test_message* msg = new service_test_message(task_length);
    tport->client_server_send(partner, srv->node_assignment(partner), srv->aid(), msg);
  }
  //send a shutdown request to server 0 - make rank 0 in charge
  if (tport->rank() == 0) tport->shutdown_server(0, srv->node_assignment(0), srv->aid());
  tport->finalize();
  return 0;
}

