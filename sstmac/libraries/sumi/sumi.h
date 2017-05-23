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

#ifndef sumi_msg_api_h
#define sumi_msg_api_h

#include <sumi/options.h>
#include <sumi/message.h>
#include <sumi/comm_functions.h>
#include <sumi/collective_message.h>
#include <sumi/timeout.h>
#include <sumi/communicator.h>
#include <sumi/thread_safe_set.h>
#include <sstmac/libraries/sumi/sumi_transport.h>

namespace sumi {

void comm_init();

void comm_finalize();

int comm_rank();

int comm_nproc();

/**
    @param dst The destination to send to
*/
void comm_send_header(int dst, const message::ptr& msg);

void comm_cancel_ping(int dst, int tag);

void comm_ping(int dst, int tag, timeout_function* func);

void comm_send_payload(int dst, const message::ptr& msg);

void comm_send(int dst, message::payload_type_t ev, const message::ptr& msg);

void comm_rdma_put(int dst, const message::ptr& msg);

void comm_rdma_get(int dst, const message::ptr& msg);

void comm_nvram_get(int dst, const message::ptr& msg);

void comm_alltoall(void* dst, void* src, int nelems,
   int type_size, int tag, bool fault_aware = false,
   int context = options::initial_context, communicator* dom = nullptr);

void comm_allgather(void* dst, void* src, int nelems,
   int type_size, int tag, bool fault_aware = false,
   int context = options::initial_context, communicator* dom = nullptr);

void comm_allgatherv(void* dst, void* src, int* recv_counts,
   int type_size, int tag, bool fault_aware = false,
   int context = options::initial_context, communicator* dom = nullptr);

void comm_gather(int root, void* dst, void* src, int nelems,
   int type_size, int tag, bool fault_aware = false,
   int context = options::initial_context, communicator* dom = nullptr);

void comm_scatter(int root, void* dst, void* src, int nelems,
   int type_size, int tag, bool fault_aware = false,
   int context = options::initial_context, communicator* dom = nullptr);

void comm_bcast(int root, void* buffer, int nelems,
   int type_size, int tag, bool fault_aware = false,
   int context = options::initial_context, communicator* dom = nullptr);

/**
* The total size of the input/result buffer in bytes is nelems*type_size
* @param dst  Buffer for the result. Can be NULL to ignore payloads.
* @param src  Buffer for the input. Can be NULL to ignore payloads.
* @param nelems The number of elements in the input and result buffer.
* @param type_size The size of the input type, i.e. sizeof(int), sizeof(double)
* @param tag A unique tag identifier for the collective
* @param fxn The function that will actually perform the reduction
* @param fault_aware Whether to execute in a fault-aware fashion to detect failures
* @param context The context (i.e. initial set of failed procs)
*/
void comm_allreduce(void* dst, void* src, int nelems, int type_size, int tag,
  reduce_fxn fxn, bool fault_aware=false, int context = options::initial_context,
  communicator* dom = nullptr);

template <typename data_t, template <typename> class Op>
void comm_allreduce(void* dst, void* src, int nelems, int tag,
    bool fault_aware = false, int context = options::initial_context, communicator* dom = nullptr){
  typedef ReduceOp<Op, data_t> op_class_type;
  comm_allreduce(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, fault_aware, context, dom);
}

void comm_scan(void* dst, void* src, int nelems, int type_size, int tag,
  reduce_fxn fxn, bool fault_aware=false, int context = options::initial_context,
  communicator* dom = nullptr);

template <typename data_t, template <typename> class Op>
void comm_scan(void* dst, void* src, int nelems, int tag,
      bool fault_aware = false, int context = options::initial_context, communicator* dom = nullptr){
  typedef ReduceOp<Op, data_t> op_class_type;
  comm_scan(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, fault_aware, context, dom);
}

void comm_reduce(int root, void* dst, void* src, int nelems, int type_size, int tag,
  reduce_fxn fxn, bool fault_aware=false, int context = options::initial_context,
  communicator* dom = nullptr);

template <typename data_t, template <typename> class Op>
void comm_reduce(int root, void* dst, void* src, int nelems, int tag,
            bool fault_aware = false, int context = options::initial_context, communicator* dom = nullptr){
  typedef ReduceOp<Op, data_t> op_class_type;
  comm_reduce(root, dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, fault_aware, context, dom);
}

void comm_barrier(int tag, bool fault_aware = false, communicator* dom = nullptr);

/**
* The total size of the input/result buffer in bytes is nelems*type_size
* This always run in a fault-tolerant fashion
* This uses a dynamic tree structure that reconnects partners when failures are detected
* @param vote The vote (currently restricted to integer) from this process
* @param nelems The number of elements in the input and result buffer.
* @param tag A unique tag identifier for the collective
* @param fxn The function that merges vote, usually AND, OR, MAX, MIN
* @param context The context (i.e. initial set of failed procs)
*/
void comm_vote(int vote, int tag, vote_fxn fxn, int context = options::initial_context, communicator* dom = nullptr);

template <template <class> class VoteOp>
void comm_vote(int vote, int tag, int context = options::initial_context, communicator* dom = nullptr){
  typedef VoteOp<int> op_class_type;
  comm_vote(vote, tag, &op_class_type::op, context, dom);
}

/**
* Helper function. Kill the node that is currently running.
* This is invoked by an application.  This allows an
* application to die at a very, very specific point in application execution.
*/
void comm_kill_node();

/**
* Helper function. Kill the process that is currently running.
* This only kills the process - it leaves the node alive and well.
*/
void comm_kill_process();

const thread_safe_set<int>& comm_failed_ranks();

const thread_safe_set<int>& comm_failed_ranks(int context);

void comm_start_heartbeat(double interval);

void comm_stop_heartbeat();

collective_done_message::ptr comm_collective_block(collective::type_t ty, int tag);

message::ptr comm_poll();

void compute(double sec);

void sleep(double sec);

void sleep_until(double sec);

/**
 * Map a physical node location to its virtual assignment in the communicator
 * @param node_id
 * @return
 */
int comm_partner(long node_id);

/**
 * Every node has exactly the same notion of time - universal, global clock.
 * Thus, if rank 0 starts and 10 minuts later rank 1 starts,
 * even though rank 1 has only been running for 30 seconds, the time will still return
 * 10 mins, 30 seconds.
 * @return The current system wall-clock time in seconds.
 *         At application launch, time is zero.
 */
double wall_time();

sstmac::sumi_transport* sumi_api();

}


#endif // SIMPMSG_H