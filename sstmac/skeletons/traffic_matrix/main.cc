/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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
#include <sstmac/main/driver.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sumi/transport.h>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "intensity", "parameter tuning the injection rate" },
{ "mixing", "the number of partners each rank has (mixing rate)" },
{ "niterations", "the number of iterations to run" },
{ "scatter", "the rank-distance between partners" },
);

MakeDebugSlot(traffic_matrix)
MakeDebugSlot(traffic_matrix_results)


static const int send_cq = sumi::Message::no_ack;
static const int RecvCQ = 0;

class sumi_param_bcaster : public sprockit::param_bcaster
{
 public:
  sumi_param_bcaster(sumi::CollectiveEngine* engine) : engine_(engine), tag_(12345) {}

  void bcast(void *buf, int size, int me, int root){
    engine_->bcast(root, buf, size, sizeof(char), tag_, sumi::Message::default_cq);
    sumi::CollectiveDoneMessage* msg = nullptr;
    auto* dmsg = engine_->blockUntilNext(sumi::Message::default_cq);
    delete dmsg;
    ++tag_;
  }

 private:
  int tag_;
  sumi::CollectiveEngine* engine_;
};

static const int window_bytes = 262144;


class config_message : public sumi::Message
{
  ImplementSerializable(config_message)

 public:
  config_message(){} //need for serialization

  template <class... Args>
  config_message(void* recv_buf, Args&&... args) :
    sumi::Message(std::forward<Args>(args)...), recv_buf_(recv_buf)
  {}

  void* recv_buf() const {
    return recv_buf_;
  }

  virtual void serialize_order(sstmac::serializer &ser) override {
    ser.primitive(recv_buf_);
    sumi::Message::serialize_order(ser);
  }

 private:
  void* recv_buf_;
};

class rdma_message :
  public sumi::Message
{
 ImplementSerializable(rdma_message)

 public:
  rdma_message(){} //need for serialization

 template <class... Args>
  rdma_message(int iter, double start_time, Args&&... args) :
   sumi::Message(std::forward<Args>(args)...),
   iter_(iter), start_(start_time)
  {
  }

  virtual void serialize_order(sstmac::serializer& ser) override {
    ser & iter_;
    ser & start_;
    ser & finish_;
    sumi::Message::serialize_order(ser);
  }

  NetworkMessage* cloneInjectionAck() const override {
    auto* cln = new rdma_message(*this);
    cln->convertToAck();;
    return cln;
  }


  int iter() const { return iter_; }

  double start() const { return start_; }
  double finish() const { return finish_; }

  void set_start(double t) { start_ = t; }
  void set_finish(double t) { finish_ = t; }

 private:
  int iter_;
  double start_;
  double finish_;
};

std::vector<std::map<int, std::map<int, rdma_message*>>> results;
static int num_done = 0;

void
progress_loop(sumi::Transport* tport, double timeout,
              std::list<rdma_message*>& done)
{
  double now = tport->wallTime();
  double stop = now + timeout;
  debug_printf(sprockit::dbg::traffic_matrix,
    "Rank %d entering progress loop at t=%10.6e - stop=%10.6e, timeout=%10.6e",
    tport->rank(), now, stop, timeout);
  while (timeout > 0){
    rdma_message* msg = dynamic_cast<rdma_message*>(tport->poll(false, timeout));
    now = tport->wallTime();
    if (msg){ //need if statement, if timed out then no message
      timeout = stop - now; //timeout shrinks
      msg->set_finish(now);
      done.push_back(msg);
      debug_printf(sprockit::dbg::traffic_matrix,
        "Rank %d got incoming message at t=%10.6e from %d",
        tport->rank(), now, msg->sender());
    } else {
      debug_printf(sprockit::dbg::traffic_matrix,
        "Rank %d timed out in progress loop at t=%10.6e",
        tport->rank(), now);
      break; //timed out!
    }
  }
  debug_printf(sprockit::dbg::traffic_matrix,
    "Rank %d exiting progress loop at t=%10.6e",
    tport->rank(), now);
}

void do_all_sends(
  int iteration,
  sumi::Transport* tport,
  int chunk_size,
  const std::vector<int>& send_partners,
  const std::vector<void*>& send_chunks,
  const std::vector<void*>& recv_chunks,
  double timeout,
  std::list<rdma_message*>& done)
{
  int npartners = send_partners.size();
  double local_timeout = (timeout / npartners) * 0.9; //fudge factor of 0.9 to lower it a bit
  for (int i=0; i < npartners; ++i){
    debug_printf(sprockit::dbg::traffic_matrix,
      "Rank %d putting to %d on iteration %d chunk of size %d: %p -> %p",
      tport->rank(), send_partners[i], 
      iteration, chunk_size,
      ((void*)send_chunks[i]), ((void*)recv_chunks[i]));
    tport->rdmaPut<rdma_message>(send_partners[i], chunk_size, send_chunks[i], recv_chunks[i],
                    send_cq, RecvCQ, sumi::Message::pt2pt, iteration, tport->wallTime());
    //stagger the sends, try to make progress on pendind messages
    progress_loop(tport, local_timeout, done);
  }
  debug_printf(sprockit::dbg::traffic_matrix,
    "Finished sending on iteration %d on rank %d at t=%10.6e",
    iteration, tport->rank(), tport->wallTime());
}

void
quiesce(sumi::Transport* tport,
  int npartners, int niterations,
  std::list<rdma_message*>& done)
{
  int ntotal = npartners * niterations;
  debug_printf(sprockit::dbg::traffic_matrix,
    "Rank %d starting quiescence: need %d, have %d p=%d n=%d",
    tport->rank(), ntotal, done.size(), npartners, niterations);
  while (done.size() < ntotal){
    rdma_message* msg = dynamic_cast<rdma_message*>(tport->poll(true));
    double now = tport->wallTime();
    msg->set_finish(now);
    done.push_back(msg);
    debug_printf(sprockit::dbg::traffic_matrix,
      "Rank %d got message in quiescence %d->%d: need %d, have %d",
      tport->rank(), msg->sender(), msg->recver(), ntotal, done.size());
  }
}

#define sstmac_app_name traffic_matrix

int USER_MAIN(int argc, char** argv)
{
  sumi::Transport* tport = sstmac::sw::OperatingSystem::currentThread()
      ->get_api<sumi::Transport>();

  tport->init();

  debug_printf(sprockit::dbg::traffic_matrix,
    "Rank %d entering initial param bcast",
    tport->rank());

  sprockit::sim_parameters::ptr params = sstmac::sw::App::getParams();

  /** This configures the compute intensity as a function of baseline bandwidth
   *  Messages are sent in windows of size 100 us
   *  The default chunk size is 256 KB for an intensity of 1.0
   *  This means an intensity of 1.0 requires 2.56GB/s to keep up */
  double intensity = params->get_double_param("intensity");

  /** This configures the number of partners each rank sends to
   *  For mixing=4 and intensity=1.0, every 100 us
   *  Each rank would send 256/4 = 64KB to every partner
   */
  int mixing = params->get_int_param("mixing");

  int num_iterations = params->get_int_param("niterations");

  /** This configures how local the traffic pattern is
   *  For scatter=1, rank N sends to N+1,N+2,etc
   *  For scatter=2, rank N sends to N+2,N+4,etc
   */
  int scatter = params->get_int_param("scatter");

  //allocate 256 replicas of the 256 KB chunk
  int npartners = mixing;
  std::vector<void*> recv_chunks(npartners);
  std::vector<void*> send_chunks(npartners);

  debug_printf(sprockit::dbg::traffic_matrix,
    "Rank %d starting run with mixing=%d, niter=%d, scatter=%d",
    tport->rank(), mixing, num_iterations, scatter);

  //because of weirdness with page boundaries,
  //only allow certain mixing numbers
  if (window_bytes % mixing != 0){
    spkt_throw_printf(sprockit::value_error,
      "invalid mixing fraction %d - mixing number must divide %d",
      mixing, window_bytes);
  }

  int me = tport->rank();
  int nproc = tport->nproc();

  results.resize(nproc);

  std::vector<int> send_partners(npartners); //I send to these guys
  std::vector<int> recv_partners(npartners); //I recv from these guys
  std::map<int, int> rank_to_send_partner_index;
  for (int i=0; i < mixing; ++i){
    int shift = (i+1)*scatter;
    send_partners[i] = (me + shift) % nproc;
    recv_partners[i] = (me + nproc - shift) % nproc;
    //printf("Rank %d: added send partner %d, recv partner %d\n",
    //  me, send_partners[i], recv_partners[i]);
    rank_to_send_partner_index[send_partners[i]] = i;
  }

  void* send_buf = nullptr;// = tport->allocate_public_buffer(window_bytes);
  void* recv_buf = nullptr;// = tport->allocate_public_buffer(window_bytes);
  int chunk_size = window_bytes / mixing;
  for (int i=0; i < npartners; ++i){
    send_chunks[i] = sumi::Message::offset_ptr(send_buf, chunk_size*i);
    recv_chunks[i] = sumi::Message::offset_ptr(recv_buf, chunk_size*i);
  }

  //send all my config messages
  for (int i=0; i < npartners; ++i){
    debug_printf(sprockit::dbg::traffic_matrix,
      "Rank %d sending config message to partner %d",
      tport->rank(), recv_partners[i]);
    tport->smsgSend<config_message>(recv_partners[i], 0, nullptr,
                                     sumi::Message::no_ack, sumi::Message::default_cq,
                                     sumi::Message::pt2pt, recv_chunks[i]);
  }

  int configs_recved = 0;
  //everyone I send to will send me a config message
  //this tells me the buffer I will put into
  debug_printf(sprockit::dbg::traffic_matrix,
    "Rank %d waiting on %d config messages from recv partners",
    tport->rank(), npartners);
  while (configs_recved < npartners){
    sumi::Message* smsg = tport->poll(true, sumi::Message::default_cq);
    config_message* msg = dynamic_cast<config_message*>(smsg);
    if (!msg){
      spkt_abort_printf("Expected config message - got %s", smsg->toString().c_str());
    }
    debug_printf(sprockit::dbg::traffic_matrix,
      "Rank %d received config message from %d",
        tport->rank(), msg->sender());
    int partner_index = rank_to_send_partner_index[msg->sender()];
    recv_chunks[partner_index] = msg->recv_buf();
    ++configs_recved;
  }

  int tag = 42;

  auto* engine = new sumi::CollectiveEngine(params, tport);
  int coll_cq = engine->tport()->allocateDefaultCq();
  engine->barrier(tag, coll_cq);
  engine->blockUntilNext(coll_cq);

  std::list<rdma_message*> done;

  double timeout = 100e-6 / intensity; //100 us per send iteration, modified by intensity
  for (int iter=0; iter < num_iterations; ++iter){
    do_all_sends(iter, tport, chunk_size, send_partners, send_chunks, recv_chunks, timeout, done);
    progress_loop(tport, timeout, done);
  }

  //finished all sends - quiesce the network
  quiesce(tport, npartners, num_iterations, done);

  std::list<rdma_message*>::iterator it, end = done.end();
  for (it=done.begin(); it != end; ++it){
    rdma_message* msg = *it;
    results[me][msg->iter()][msg->sender()] = msg;
  }
  ++num_done;

  int nresults = nproc*num_iterations*npartners;
  if (num_done == nproc){
    double* resultsArr = sstmac::SimulationQueue::allocateResults(nresults);
    int result_idx = 0;
    for (int p=0; p < nproc; ++p){
      for (int i=0; i < num_iterations; ++i){
        std::map<int, rdma_message*>& done = results[p][i];
        std::map<int, rdma_message*>::iterator it, end = done.end();
        for (it = done.begin(); it != end; ++it, ++result_idx){
          rdma_message* msg = it->second;
          double delta_t = msg->finish() - msg->start();
          double throughput_gbs = msg->byteLength() / delta_t / 1e9;
          resultsArr[result_idx] = throughput_gbs;
          debug_printf(sprockit::dbg::traffic_matrix_results,
            "Message iter=%3d source=%5d dest=%d throughput=%10.4fGB/s start=%8.4ems stop=%8.4ems",
            msg->iter(), msg->sender(), msg->recver(), throughput_gbs,
            msg->start()*1e3, msg->finish()*1e3);
       }
     }
   }
   sstmac::SimulationQueue::publishResults();
   num_done = 0;
 }
 tport->finish();
 return 0;
}
