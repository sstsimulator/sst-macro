/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sprockit/sim_parameters.h>
#include <sprockit/debug.h>
#include <sstmac/common/sstmac_env.h>

#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/libraries/pthread/sstmac_pthread.h>
#include <sumi/transport.h>
#include <sumi/sim_transport.h>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>
#include <random>
#include <chrono>

MakeDebugSlot(offered_load)

static const int send_cq = sumi::Message::no_ack;
static const int RecvCQ = 0;

class RdmaMessage :
  public sumi::Message
{
 ImplementSerializable(RdmaMessage)

 public:
  RdmaMessage(){} //need for serialization

 template <class... Args>
  RdmaMessage(int iter, double start_time, Args&&... args) :
   sumi::Message(std::forward<Args>(args)...),
   iter_(iter), start_(start_time)
  {
  }

  void serialize_order(sstmac::serializer& ser) override {
    ser & iter_;
    ser & start_;
    ser & finish_;
    sumi::Message::serialize_order(ser);
  }

  NetworkMessage* cloneInjectionAck() const override {
    auto* cln = new RdmaMessage(*this);
    cln->convertToAck();;
    return cln;
  }


  int iter() const { return iter_; }

  sstmac::Timestamp start() const { return start_; }
  sstmac::Timestamp finish() const { return finish_; }

  void setStart(sstmac::Timestamp t) { start_ = t; }
  void setFinish(sstmac::Timestamp t) { finish_ = t; }

 private:
  int iter_;
  sstmac::Timestamp start_;
  sstmac::Timestamp finish_;
};

void do_send(
  int iteration,
  sumi::Transport* tport,
  int chunk_size,
  int recver)
{
  debug_printf(sprockit::dbg::offered_load,
    "Rank %d putting to %d on iteration %d chunk of size %d",
    tport->rank(), recver,
    iteration, chunk_size);
  RdmaMessage* msg = tport->rdmaPut<RdmaMessage>(recver, chunk_size, nullptr, nullptr,
                  send_cq, RecvCQ, sumi::Message::pt2pt, 0/*qos*/, iteration, tport->wallTime());
  msg->setStart(tport->now());
}

struct RunParams {
  sumi::Transport* tport;
  int npartners;
  int niterations;
};

void* run_completion(void* args)
{
  RunParams* params = (RunParams*) args;

  int ntotal = params->npartners * params->niterations;
  debug_printf(sprockit::dbg::offered_load,
    "Rank %d starting quiescence: need %d",
    params->tport->rank(), ntotal);
  int recved = 0;
  while (recved < ntotal){
    RdmaMessage* msg = dynamic_cast<RdmaMessage*>(params->tport->poll(true));
    sstmac::TimeDelta send_time = params->tport->now() - msg->start();
    printf("Message %d->%d on iteration %d of size %d took %14.10fs\n",
           msg->sender(), params->tport->rank(), msg->iter(), int(msg->byteLength()),
           send_time.sec());
    ++recved;
    debug_printf(sprockit::dbg::offered_load,
      "Rank %d got message in quiescence %d->%d: need %d, have %d",
      params->tport->rank(), msg->sender(), msg->recver(), ntotal, recved);
  }

  return nullptr;
}


#define sstmac_app_name offered_load
int USER_MAIN(int  /*argc*/, char**  /*argv*/)
{

  sumi::Transport* tport = sumi::Transport::get();
  tport->init();

  sstmac::sw::Thread* thr = sstmac::sw::Thread::current();

  debug_printf(sprockit::dbg::offered_load,
    "Rank %d entering initial param bcast",
    tport->rank());

  SST::Params params = sstmac::sw::App::getParams();

  int message_size = params.find<SST::UnitAlgebra>("message_size").getRoundedValue();
  sstmac::TimeDelta constant_delay(params.find<SST::UnitAlgebra>("constant_delay").getValue().toDouble());
  sstmac::TimeDelta mean_variable_delay(params.find<SST::UnitAlgebra>("variable_delay").getValue().toDouble());
  double lambda_inv = mean_variable_delay.sec();

  std::vector<int> destinations; params.find_array("destinations", destinations);

  int my_destination = destinations[tport->rank()];
  //figure out everyone sending to me
  std::vector<int> senders_to_me;
  for (int dest : destinations){
    if (dest == tport->rank()){
      senders_to_me.push_back(dest);
    }
  }
  int npartners = senders_to_me.size();

  int num_iterations = params.find<int>("niterations");

  int tag = 42;
  auto* engine = new sumi::CollectiveEngine(params, tport);
  int coll_cq = engine->tport()->allocateDefaultCq();
  engine->barrier(tag, coll_cq);
  engine->blockUntilNext(coll_cq);

  RunParams rparams;
  rparams.tport = tport;
  rparams.niterations = num_iterations;
  rparams.npartners = npartners;
  sstmac_pthread_t cthr;
  SSTMAC_pthread_create(&cthr, nullptr, run_completion, &rparams);

  std::mt19937_64 rng;
  // initialize the random number generator with time-dependent seed
  uint64_t timeSeed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
  std::seed_seq ss{uint32_t(timeSeed & 0xffffffff), uint32_t(timeSeed>>32)};
  rng.seed(ss);
  // initialize a uniform distribution between 0 and 1
  std::uniform_real_distribution<double> dist(0., 1.);

  for (int iter=0; iter < num_iterations; ++iter){
    do_send(iter, tport, message_size, my_destination);
    //a poission process is generated by exponentially distributed delays
    sstmac::TimeDelta variable_delay( -log(dist(rng)) * lambda_inv );
    sstmac::TimeDelta total_delay = variable_delay + constant_delay;
    thr->os()->sleep(total_delay);
  }

  void* status;
  SSTMAC_pthread_join(cthr, &status);

 tport->finish();
 return 0;
}
