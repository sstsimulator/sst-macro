#include <sstmac/libraries/sumi/distributed_service.h>
#include <sstmac/software/launch/app_launch.h>
#include <sumi/transport.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/skeleton.h>
#include <sstmac/util.h>

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

  std::string
  to_string() const {
    return "service test message";
  }

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
  msg = busy_loop(tport);
  int rank = tport->rank();
  while (msg) {
    service_test_message::ptr smsg = ptr_safe_cast(service_test_message, msg);
    printf("Service node %d sleeping for %8.4es\n", rank, smsg->workload.sec());
    sleep(smsg->workload);
    msg = busy_loop(tport);
  }
}

SpktRegister("test_service", sw::app, test_service);

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

  auto nodes = sstmac::sw::app_launch::nodes("test_service");
  int num_service_nodes = nodes.size();
  int me = tport->rank();
  int partner = me;
  std::string target_service = "test_service";
  for (int i=0; i < num_messages; ++i){
    partner = ((partner + 5) * 7) % num_service_nodes;
    printf("Client %d sending to service node %d at addr %d\n", me, partner, nodes[partner]);
    service_test_message* msg = new service_test_message(task_length);
    tport->client_server_send(target_service, partner, nodes[partner], msg);
  }

  tport->finalize();
  return 0;
}

