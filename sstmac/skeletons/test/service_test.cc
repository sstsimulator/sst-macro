#include <sstmac/libraries/sumi/distributed_service.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/skeleton.h>
#include <sumi/transport.h>
#include <sstmac/libraries/sumi/sumi.h>

namespace sstmac {

class test_service : public distributed_service
{
 public:
  test_service(sprockit::sim_parameters* params, sw::software_id sid) :
    distributed_service(params, sid)
  {
  }

  void run(sumi::transport *tport);
};

class service_test_message : public sumi::message
{
 public:
  typedef sprockit::refcount_ptr<service_test_message> ptr;

  service_test_message(timestamp w) :
    sumi::message(100), workload(w)
  {
  }

  timestamp workload;
};

void
test_service::run(sumi::transport *tport)
{
  sumi::message::ptr msg;
  //now go into polling loop
  while ((msg = busy_loop(tport))){
    service_test_message::ptr smsg = ptr_safe_cast(service_test_message, smsg);
    sleep(smsg->workload);
  }

}

SpktRegister("test_service", sw::app, test_service);

//#define sstmac_app_name test_client

int themain(int argc, char** argv)
{
  sprockit::sim_parameters* params = get_params();
  int num_messages = params->get_optional_int_param("num_tasks", 10);
  timestamp task_length(1e-3);

  sumi::transport* tport = sumi::sumi_api();
  tport->init();

  auto nodes = sstmac::sw::app_launch::nodes("test_service");
  int num_service_nodes = nodes.size();
  int me = tport->rank();
  int partner = me;
  for (int i=0; i < num_messages; ++i){
    partner = ((partner + 5) * 7) % num_service_nodes;
    printf("Client %d sending to service node %d at addr %d", me, partner, nodes[partner]);
    service_test_message* msg = new service_test_message(task_length);
    tport->send_header(partner, msg);
  }

  tport->finalize();
  return 0;
}

}
