#include <sprockit/test/test.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/libraries/sumi/sumi_api.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/hardware/topology/traffic/traffic.h>
#include <sstmac/hardware/topology/topology.h>
#include <sumi/transport.h>
#include <sstmac/skeleton.h>

using namespace sumi;

static long done = 0;

#define sstmac_app_name user_app_cxx

void run_test(
  sstmac::hw::traffic_pattern::type_t ty,
  long num_bytes
)
{
  std::vector<sstmac::node_id> node_partners;
  sstmac::sumi_api* simp = safe_cast(sstmac::sumi_api, sumi_api());
  sstmac::hw::topology::global()->send_partners(
    ty,
    simp->my_addr(),
    node_partners);
  int num_partners = node_partners.size();
  std::vector<int> send_partners(num_partners, 0);
  for (size_t i=0; i < node_partners.size(); ++i) {
    send_partners[i] = simp->get_partner(node_partners[i]);
  }

  node_partners.clear();
  std::vector<int> recv_partners(num_partners, 0);
  sstmac::hw::topology::global()->recv_partners(
    ty,
    simp->my_addr(),
    node_partners);
  for (int i=0; i < num_partners; ++i) {
    recv_partners[i] = simp->get_partner(node_partners[i]);
  }


  for (int i=0; i < num_partners; ++i) {
    message::ptr msg = new message(num_bytes);
    comm_rdma_put(send_partners[i], msg);
  }

  int num_recved = 0;
  while (num_recved < num_partners) {
    message::ptr msg = comm_poll();
    if (msg->payload_type() == message::rdma_put_ack) {
      //ignore
    }
    else if (msg->payload_type() == message::rdma_put) {
      ++num_recved;
    }
    else {
      spkt_throw_printf(sprockit::illformed_error,
                       "got unexpected message %s",
                       message::tostr(msg->payload_type()));
    }
  }
  done++;
}

int
main(int argc, char** argv)
{
  comm_init();

  sprockit::sim_parameters* params = sstmac::sw::app::get_params();

  std::string pattern = params->get_param("traffic_pattern");
  sstmac::hw::traffic_pattern::type_t ty;
  if (pattern == "NN" || pattern == "nearest_neighbor") {
    ty = sstmac::hw::traffic_pattern::nearest_neighbor;
  }
  else if (pattern == "BC" || pattern == "bit_complement") {
    ty = sstmac::hw::traffic_pattern::bit_complement;
  }
  else if (pattern == "TOR" || pattern == "tornado") {
    ty = sstmac::hw::traffic_pattern::tornado;
  }
  else {
    spkt_throw_printf(sprockit::input_error,
                     "invalid traffic pattern %s",
                     pattern.c_str());
  }

  long num_bytes_per_msg = params->get_byte_length_param("message_length");

  run_test(ty, num_bytes_per_msg);

  int rank = comm_rank();

  comm_finalize();

  if (rank == 0) {
    std::cout << "FINISHED" << std::endl;
  }
  return 0;
}


