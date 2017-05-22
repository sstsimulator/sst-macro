/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sprockit/test/test.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sumi/transport.h>
#include <sstmac/libraries/sumi/sumi_api.h>
#include <sstmac/libraries/sumi/sumi_thread.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/hardware/topology/traffic/traffic.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name user_app_cxx
using namespace sumi;
using sstmac::timestamp;
using sstmac::hw::traffic_pattern;
using sstmac::node_id;
using sstmac::env;
using sstmac::hw::topology;

RegisterKeywords(
"traffic_pattern",
"inject_time",
"network_injector_capacity_bw"
);

//static long done = 0;

static int num_finished = 0;
static double average_latency_ms = 0;
static long num_messages_counted = 0;
//static long short_msg_length = 8000;
static double latency_total = 0;
typedef spkt_unordered_map<sumi::message*,double> time_map;
typedef spkt_unordered_map<int, time_map> rank_time_map;
static rank_time_map start_times;


class throughput_thread :
  public sstmac::sumi_thread
{
 public:
  throughput_thread(sstmac::sw::software_id sid) :
    sstmac::sumi_thread(sid){}

  virtual void run();

  ~throughput_thread(){}
};

void
throughput_thread::run()
{
  //int num_recved = 0;
  while (1) {
    message::ptr msg = comm_poll();
    if (msg->payload_type() == message::rdma_put_ack) {
      //ignore
    }
    else if (msg->payload_type() == message::rdma_put) {
      time_map& times = start_times[msg->sender()];
      time_map::iterator it = times.find(msg.get());
      if (it == times.end()) {
        continue; //ignore the first message
      }

      double start_time = it->second;
      times.erase(it);
      double now = wall_time();
      double delta = now - start_time;
      double delta_ms = delta * 1e3;
      latency_total += delta_ms;
      num_messages_counted++;
      average_latency_ms = latency_total / num_messages_counted;
      return;
    }
    else {
      spkt_throw_printf(sprockit::illformed_error,
                       "got unexpected message %s",
                       sumi::message::tostr(msg->payload_type()));
    }
  }

}

void run_test(
  traffic_pattern::type_t ty,
  long inject_length,
  double offered_load_bw
)
{
  std::vector<node_id> node_partners;
  sstmac::sumi_api* simp = safe_cast(sstmac::sumi_api, sumi_api());
  topology::global()->send_partners(
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
  topology::global()->recv_partners(
    ty,
    simp->my_addr(),
    node_partners);
  for (int i=0; i < num_partners; ++i) {
    recv_partners[i] = simp->get_partner(node_partners[i]);
  }

  int aid = 1; //assume 1 for now
  sstmac::sw::software_id sid(aid, comm_rank());
  throughput_thread* thr = new throughput_thread(sid);
  thr->start();

  int me = comm_rank();
  for (int i=0; i < num_partners; ++i) {
    sumi::message::ptr msg = new sumi::message(inject_length);
    comm_rdma_put(send_partners[i], msg);

    // sleep until the message WOULD be done
    // injecting in the absence of congestion
    double delay = inject_length / offered_load_bw;
    sleep(delay);

    //now send a single, small message
    msg = new sumi::message(8000);
    comm_rdma_put(send_partners[i], msg);
    start_times[me][msg.get()] = wall_time();
  }

  thr->join();
}

int
main(int argc, char** argv)
{
  comm_init();

  sprockit::sim_parameters* params = sstmac::sw::app::get_params();

  std::string pattern = params->get_param("traffic_pattern");
  traffic_pattern::type_t ty;
  if (pattern == "NN" || pattern == "nearest_neighbor") {
    ty = traffic_pattern::nearest_neighbor;
  }
  else if (pattern == "BC" || pattern == "bit_complement") {
    ty = traffic_pattern::bit_complement;
  }
  else if (pattern == "TOR" || pattern == "tornado") {
    ty = traffic_pattern::tornado;
  }
  else {
    spkt_throw_printf(sprockit::input_error,
                     "invalid traffic pattern %s",
                     pattern.c_str());
  }

  double offered_load_bw = 0;

  if (params->has_param("pisces_injection_bandwidth")) {
    offered_load_bw = params->get_bandwidth_param("pisces_injection_bandwidth");
  }
  else if (params->has_param("cycle_accurate_switch_bandwidth_n2r")) {
    offered_load_bw = params->get_bandwidth_param("cycle_accurate_switch_bandwidth_n2r");
  }
  else if (params->has_param("network_injector_capacity_bw")) {
    offered_load_bw = params->get_bandwidth_param("network_injector_capacity_bw");
  }
  else if (params->has_param("packet_switch_bandwidth_n2r")) {
    offered_load_bw = params->get_bandwidth_param("packet_switch_bandwidth_n2r");
  }
  else if (params->has_param("network_train_injection_bw")) {
    offered_load_bw = params->get_bandwidth_param("network_train_injection_bw");
  }
  else {
    spkt_throw_printf(sprockit::input_error,
                     "throughput application did not find injection bandwidth");
  }

  timestamp inject_time = params->get_time_param("inject_time");
  long inject_length = offered_load_bw * inject_time.sec();

  run_test(ty, inject_length, offered_load_bw);

  //int rank = comm_rank();
  int nproc = comm_nproc();

  comm_finalize();

  ++num_finished;

  if (num_finished == nproc) {
    std::cout << sprockit::printf("Average latency: %10.5f ms\n", average_latency_ms);
  }
  return 0;
}


