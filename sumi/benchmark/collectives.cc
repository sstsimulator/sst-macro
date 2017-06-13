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

#include <pthread.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/stl_string.h>
#include <unistd.h>

#define DEBUG 0

using namespace sumi;

typedef enum {
  allgather,
  allreduce
} type_t;

static const int nreplica = 15;

void
run_test(const std::string& test, transport* t, communicator* dom, int nelems, int context, int& tag)
{
  if (t->rank() >= dom->nproc()){
    ++tag; //gotta increment this thought to stay consistent
    return; //I have no part in this
  }

  int nproc = dom->nproc();


  double t_start;
  int *src_buf = nullptr, *dst_buf = nullptr, *reduce_buf = nullptr;
  if (test == "allgather"){
    src_buf = (int*) ::malloc(sizeof(int)*nelems);
    dst_buf = (int*) ::malloc(sizeof(int)*nelems*nproc);
    ::memset(src_buf, 0, nelems*sizeof(int));
    ::memset(dst_buf, 0, nelems*nproc*sizeof(int));
    t_start = t->wall_time();
    t->allgather(dst_buf, src_buf, nelems, sizeof(int), tag, true, context, dom);
  } else if (test == "vote"){
    t_start = t->wall_time();
    t->vote<AndOp>(1, tag, context, dom);
  } else if (test == "allreduce"){
    reduce_buf = (int*) ::malloc(sizeof(int)*nelems);
    ::memset(reduce_buf, 0, nelems*sizeof(int));
    t_start = t->wall_time();
    t->allreduce<int,Add>(reduce_buf, reduce_buf, nelems, tag, true, context, dom);
  }
  message::ptr msg = t->blocking_poll();
  double t_stop = t->wall_time();
  double t_total = t_stop - t_start;
  if (src_buf) ::free(src_buf);
  if (dst_buf) ::free(dst_buf);
  if (reduce_buf) ::free(reduce_buf);

  if (dom->my_comm_rank() == 0){
    printf("Test %s: nelems=%d nproc=%d t=%20.12f ms\n",
      test.c_str(), nelems, nproc, t_total*1e3);
  }

  ++tag;
}

void
run_test(transport* t, communicator* dom, int& tag, int* nelems, int ntests, const char* name)
{
  for (int i=0; i < ntests; ++i){
    for (int r=0; r < nreplica; ++r){
      run_test(name, t, dom, nelems[i], options::initial_context, tag);
    }
  }
}

void
run_test(transport* t, communicator* dom, int& tag)
{
  int reduce_nelems[] = { 64, 256, 1024, 4096, 16384 };
  int allgather_nelems[] = { 32, 64, 128, 512, 1024 };
  int vote_nelems[] = {1,1,1,1,1};
  int ntests = sizeof(reduce_nelems) / sizeof(int);
 
  run_test(t, dom, tag, reduce_nelems, ntests, "allreduce");
  run_test(t, dom, tag, allgather_nelems, ntests, "allgather");
  run_test(t, dom, tag, vote_nelems, ntests, "vote");

}

void
run_test(transport* t, int& tag)
{
  int me = t->rank();
  int nproc = t->nproc();
  int domain_nproc = nproc;

  while (domain_nproc >= 4){
    communicator* dom = new subrange_communicator(me, 0, domain_nproc);
    run_test(t,dom,tag);
    domain_nproc /= 2;
  }
}

void
run_vote_test(const char* name, transport* t, communicator* dom, int num_failures, int& tag)
{
  if (dom->nproc() <= t->rank()){
    return;
  }

  double t_start = t->wall_time();
  t->vote<AndOp>(1, tag, options::initial_context, dom);
  collective_done_message::ptr dmsg = ptr_safe_cast(collective_done_message, t->blocking_poll());
  double t_stop = t->wall_time();
  double t_total = t_stop - t_start;
  if (t->rank() == 0){
    printf("Test %s: nfailures=%d nproc=%d t=%20.12f ms for failed=%s\n",
      name, num_failures, dom->nproc(), t_total*1e3,
      dmsg->failed_procs().to_string().c_str());
  }

}

void
run_vote_test(const char* name, transport *t, int num_failures, int &tag)
{
  int nproc = t->nproc();
  while (nproc >= 4){
    communicator* dom = new subrange_communicator(t->rank(), 0, nproc);
    nproc /= 2;
    run_vote_test(name, t, dom, num_failures, tag);
    ++tag;
  }
}

void
run_vote_test(const char* name, transport* t, int max_failures, int* failures, int& tag)
{
  for (int i=0; i < max_failures; ++i){
    int to_fail = failures[i];
    if (t->rank() == to_fail){
      printf("Node %d going down\n", to_fail);
      t->die();
      t->blocking_poll(); //block until I get a terminate message
      t->revive();
      break;
    } else {
      int num_failures = i + 1;
      for (int r=0; r < nreplica; ++r){
        run_vote_test(name, t, num_failures, tag);
      }
    }
  }
  t->clear_failures();
  if (t->rank() == 0){
    for (int i=0 ; i < max_failures; ++i){
      t->send_terminate(failures[i]);
    }
  }

  //make sure that the dead guys wake up before hitting the barrier
  sleep(1);

  /** Make sure everyone got here */
  t->barrier(123456789, false);
  t->blocking_poll();
}

void
run_test()
{
  sprockit::sim_parameters params;
  params["transport"] = DEFAULT_TRANSPORT;
  params["ping_timeout"] = "100ms";
  params["eager_cutoff"] = "512";

  params["use_put_protocol"] = "false";
  params["lazy_watch"] = "true";
  transport* t = transport::factory::get_param("transport", &params);

  t->init();

  int tag = 0;

  if (t->rank() == 0) printf("Eager=512 Lazy=True Protocol=Get Ack=Software\n");
  t->set_use_hardware_ack(false);
  t->set_put_protocol(false);
  run_test(t, tag);

  if (t->rank() == 0) printf("Eager=512 Lazy=True Protocol=Put Ack=Software\n");
  t->set_use_hardware_ack(false);
  t->set_put_protocol(true);
  run_test(t, tag);

  if (t->supports_hardware_ack()){
    if (t->rank() == 0) printf("Eager=512 Lazy=True Protocol=Get Ack=Hardware\n");
    t->set_use_hardware_ack(true);
    t->set_put_protocol(false);
    run_test(t, tag);

    if (t->rank() == 0) printf("Eager=512 Lazy=True Protocol=Put Ack=Hardware\n");
    t->set_use_hardware_ack(true);
    t->set_put_protocol(true);
    run_test(t, tag);
  }



  //now let's do some tests with dead procs
  int max_num_failed_procs = 0;
  int nproc = t->nproc();
  while (nproc >= 4){
    ++max_num_failed_procs;
    nproc /= 4;
  }

  int random_failed_procs[] = {
    2, //4 procs
    11, //16 procs
    36, //64 procs
    106, //256 procs
    835, //1024 procs
    2347 //4096 procs
  };
  run_vote_test("random", t, max_num_failed_procs, random_failed_procs, tag);

  tag = 123456790;
  int seq_failed_procs[] = {
    1, 3, 7, 15, 31, 63, 127
  };
  run_vote_test("sequential", t, max_num_failed_procs, seq_failed_procs, tag);

  t->finish();
}

int main(int argc, char** argv)
{
  try {
#if DEBUG
  sprockit::debug::turn_on(DEFAULT_TRANSPORT);
  sprockit::debug::turn_on("sumi");
  sprockit::debug::turn_on("sumi_collective");
#endif

    run_test();
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    abort();
  }

  return 0;
}