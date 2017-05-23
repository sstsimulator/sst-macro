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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/launch_request.h>
#include <sstmac/skeleton.h>
#include <sstmac/util.h>
#include <sumi/transport.h>
#include <sstmac/libraries/sumi/sumi.h>
#include <sstmac/libraries/sumi/distributed_service.h>
#include <sstmac/software/launch/job_launcher.h>

namespace sstmac {

class test_service : public distributed_service
{
  ServiceRegister("test_service", test_service)
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

  std::string to_string() const override {
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

  sstmac::sw::task_mapping::ptr srv = sstmac::sw::task_mapping::global_mapping("test_service");
  int num_service_nodes = srv->nproc();
  int me = tport->rank();
  int partner = me;
  for (int i=0; i < num_messages; ++i){
    partner = ((partner + 5) * 7) % num_service_nodes;
    printf("Client %d sending to service node %d at addr %d\n",
           me, partner, srv->rank_to_node(partner));
    service_test_message* msg = new service_test_message(task_length);
    tport->client_server_send(partner, srv->rank_to_node(partner), srv->aid(), msg);
  }
  //send a shutdown request to server 0 - make rank 0 in charge
  if (tport->rank() == 0) tport->shutdown_server(0, srv->rank_to_node(0), srv->aid());
  tport->finish();
  return 0;
}