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

#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sprockit/util.h>
#include <sumi/transport.h>

using namespace sstmac;
using namespace sstmac::sw;


//this redirection macro foobars things here
#ifdef sleep
#if sleep == sstmac_sleep
#undef sleep
#endif
#endif

namespace sumi {

static transport* current_transport()
{
  thread* t = thread::current();
  return t->get_api<transport>();
}

static collective_engine* current_engine()
{
  auto* tport = current_transport();
  tport->make_engine();
  return tport->engine();
}

transport* sumi_api()
{
  return current_transport();
}

collective_engine* sumi_engine()
{
  return current_engine();
}

void comm_init()
{
  auto* tport = current_transport();
  tport->init();
}

void comm_kill_process()
{
  sprockit::abort("unimplemented: comm kill process");
}

void comm_kill_node()
{
  sstmac::sw::operating_system::current_os()->kill_node();
  throw terminate_exception();
}

void comm_finalize()
{
  current_transport()->finish();
}

void
comm_allreduce(void *dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
               int cq_id, communicator* comm)
{
  current_engine()->allreduce(dst, src, nelems, type_size, tag, fxn, cq_id, comm);
}

void comm_scan(void *dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
               int cq_id, communicator* comm)
{
  current_engine()->scan(dst, src, nelems, type_size, tag, fxn, cq_id, comm);
}

void
comm_reduce(int root, void *dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
            int cq_id, communicator* comm)
{
  current_engine()->reduce(root, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
}

void comm_alltoall(void *dst, void *src, int nelems, int type_size, int tag, int cq_id, communicator* comm)
{
  current_engine()->alltoall(dst, src, nelems, type_size, tag, cq_id, comm);
}

void comm_allgather(void *dst, void *src, int nelems, int type_size, int tag, int cq_id, communicator* comm)
{
  current_engine()->allgather(dst, src, nelems, type_size, tag, cq_id, comm);
}

void comm_allgatherv(void *dst, void *src, int* recv_counts, int type_size, int tag, int cq_id, communicator* comm)
{
  current_engine()->allgatherv(dst, src, recv_counts, type_size, tag, cq_id, comm);
}

void comm_gather(int root, void *dst, void *src, int nelems, int type_size, int tag, int cq_id, communicator* comm)
{
  current_engine()->gather(root, dst, src, nelems, type_size, tag, cq_id, comm);
}

void comm_scatter(int root, void *dst, void *src, int nelems, int type_size, int tag, int cq_id, communicator* comm)
{
  current_engine()->scatter(root, dst, src, nelems, type_size, tag, cq_id, comm);
}

void comm_bcast(int root, void *buffer, int nelems, int type_size, int tag, int cq_id, communicator* comm)
{
  current_engine()->bcast(root, buffer, nelems, type_size, tag, cq_id, comm);
}

void comm_barrier(int tag, int cq_id, communicator* comm)
{
  current_engine()->barrier(tag, cq_id, comm);
}

int comm_rank()
{
  return current_transport()->rank();
}

int comm_nproc()
{
  return current_transport()->nproc();
}

message* comm_poll()
{
  return current_transport()->blocking_poll(message::default_cq);
}

double wall_time()
{
  return operating_system::current_os()->now().sec();
}

void sleep_until(double sec)
{
  thread* thr = thread::current();
  app* my_app = thr->parent_app();
  double time = sec - my_app->now().sec();
  my_app->sleep(timestamp(time));
}

void sleep(double sec)
{
  thread* thr = thread::current();
  app* my_app = thr->parent_app();
  my_app->sleep(timestamp(sec));
}

void compute(double sec)
{
  thread* thr = thread::current();
  app* my_app = thr->parent_app();
  my_app->compute(timestamp(sec));
}

}
