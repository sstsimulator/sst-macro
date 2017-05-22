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

#define DEBUG 0
#define HAVE_MPI 1

#if HAVE_MPI
#include <mpi.h>
#endif

using namespace sumi;

typedef enum {
  allgather,
  allreduce
} type_t;

static const int nreplica = 15;

static inline void
run_test_sender_get(transport* t, public_buffer pbuf, int size)
{
  rdma_message::ptr msg = new rdma_message(size);
  msg->remote_buffer() = pbuf;
  int dst = 1;
  t->send_rdma_header(dst, msg);
  message::ptr ack = t->blocking_poll(message::rdma_get_ack);
}

static inline void
run_test_receiver_get(transport* t, public_buffer pbuf, int size)
{
  message::ptr rdma_header = t->blocking_poll(message::header);
  rdma_header->local_buffer() = pbuf;
  int dst = 0;
  t->rdma_get(dst, rdma_header, true, true);
  message::ptr ack = t->blocking_poll(message::rdma_get);
}

static int num_calls = 0;

static inline void
run_test_receiver_put(transport* t, public_buffer pbuf, int size)
{
  rdma_message::ptr msg = new rdma_message(size);
  msg->remote_buffer() = pbuf;
  int dst = 0;
  t->send_rdma_header(dst, msg);
  message::ptr ack = t->blocking_poll(message::rdma_put);
  ++num_calls;
}

static inline void
run_test_sender_put(transport* t, public_buffer pbuf, int size)
{
  message::ptr rdma_header = t->blocking_poll(message::header);
  rdma_header->local_buffer() = pbuf;
  int dst = 1;
  t->rdma_put(dst, rdma_header, true, true);
  message::ptr ack = t->blocking_poll(message::rdma_put_ack);
  ++num_calls;
}

#if HAVE_MPI
static inline void
run_test_mpi_sender(transport* t, public_buffer pbuf, int size)
{
  int dst = 1;
  int tag = 0;
  MPI_Send(pbuf.ptr, size, MPI_BYTE, dst, tag, MPI_COMM_WORLD);
}

static inline void
run_test_mpi_receiver(transport* t, public_buffer pbuf, int size)
{
  int src = 0;
  int tag = 0;
  MPI_Recv(pbuf.ptr, size, MPI_BYTE, src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}
#endif

typedef void (*pp_fxn)(transport* t, public_buffer pbuf, int size);

void
run_test(transport* t, int size, int nblocks, int nrepeats, pp_fxn fxn, const char* name, bool prereg, bool loopreg, bool loopunreg)
{
  std::list<public_buffer> blocks;
  for (int i=0; i < nblocks; ++i){
    blocks.push_back(public_buffer(::malloc(size)));
    ::memset(blocks.back(), 0, size);
  }

  if (prereg){
    std::list<public_buffer>::iterator it, end = blocks.end();
    for (it=blocks.begin(); it != end; ++it){
      public_buffer& buf = *it;
      buf = t->make_public_buffer(buf.ptr, size);
    }
  }

  for (int i=0; i < nrepeats; ++i){
    double t_start = t->wall_time();
    std::list<public_buffer>::iterator it, end = blocks.end();
    for (it=blocks.begin(); it != end; ++it){
      if (loopreg){
        public_buffer& pbuf = *it;
        pbuf = t->make_public_buffer(pbuf.ptr,size);
        (*fxn)(t, pbuf, size);
        if (loopunreg) t->unmake_public_buffer(pbuf,size);
      } else {
        public_buffer& pbuf = *it;
        (*fxn)(t, pbuf, size);
      }
    }
    double t_stop = t->wall_time();
    double t_total = t_stop - t_start;
    double t_per = t_total / nblocks;
    double throughput = size / t_per;
    printf("%25s: size=%6d repeat=%3d time=%20.12fms throughput=%20.12fGB/s\n", name, size, i, t_per*1e3, throughput/1e9);
  }

  if (prereg || (loopreg && !loopunreg)){
    std::list<public_buffer>::iterator it, end = blocks.end();
    for (it=blocks.begin(); it != end; ++it){
      public_buffer& pbuf = *it;
      t->unmake_public_buffer(pbuf, size);
    }
  }
}

void
run_test(transport* t, pp_fxn fxn, const char* name, bool prereg, bool loopreg, bool loopunreg)
{
  static const int nblocks = 100;
  static const int nrepeats = 10;
  int sizes[] = {1024, 4096, 8192, 16384, 32768, 65532, 131072};
  int num_sizes = sizeof(sizes) / sizeof(int);
  for (int i=0; i < num_sizes; ++i){
    run_test(t, sizes[i], nblocks, nrepeats, fxn, name, prereg, loopreg, loopunreg);
  }
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

#if HAVE_MPI
  int argc = 1;
  char* argv[] = {"haha"};
  char** tmp = (char**) argv;
  MPI_Init(&argc,&tmp);
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  if (rank == 0){
    run_test(t, run_test_mpi_sender, "sender mpi", false, false, false);
  } else {
    run_test(t, run_test_mpi_receiver, "receiver mpi", false, false, false);
  }
  MPI_Finalize();
#else
  t->init();
  if (t->rank() == 0){
    run_test(t, run_test_sender_get, "sender get", false, true, true);
    run_test(t, run_test_sender_put, "sender put", false, true, true);
    run_test(t, run_test_sender_get, "sender get pre-register", true, false, false);
    run_test(t, run_test_sender_put, "sender put pre-register", true, false, false);
    run_test(t, run_test_sender_put, "sender put delay unreg", false, true, false);
    if (t->supports_hardware_ack()){
      t->set_use_hardware_ack(true);
      run_test(t, run_test_sender_put, "sender put ack", true, false, false);
    }
  } else {
    run_test(t, run_test_receiver_get, "receiver get", false, true, true);
    run_test(t, run_test_receiver_put, "receiver put", false, true, true);
    run_test(t, run_test_receiver_get, "receiver get pre-register", true, false, false);
    run_test(t, run_test_receiver_put, "receiver put pre-register", true, false, false);
    run_test(t, run_test_receiver_put, "receiver put delay unreg", false, true, false);
    if (t->supports_hardware_ack()){
      t->set_use_hardware_ack(true);
      run_test(t, run_test_receiver_put, "receiver put ack", false, true, false);
    }
  }

  t->finish();
#endif
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