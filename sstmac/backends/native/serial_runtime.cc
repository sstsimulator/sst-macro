#include <sstmac/backends/native/serial_runtime.h>
#include <cstring>

namespace sstmac {
namespace native {

SpktRegister("serial", parallel_runtime, serial_runtime);

serial_runtime::serial_runtime(sprockit::sim_parameters* params)
  : parallel_runtime(params, 0, 1)
{
}

int64_t
serial_runtime::allreduce_min(int64_t mintime)
{
  return mintime;
}

int64_t
serial_runtime::allreduce_max(int64_t maxtime)
{
  return maxtime;
}

void
serial_runtime::gather(void *send_buffer, int num_bytes, void *recv_buffer, int root)
{
  if (root != 0){
    spkt_throw(sprockit::value_error,
        "serial_runtime::gather: received gather request for non-zero root - there is only one proc");
  }
  ::memcpy(recv_buffer, send_buffer, num_bytes);
}

void
serial_runtime::allgather(void *send_buffer, int num_bytes, void *recv_buffer)
{
  ::memcpy(recv_buffer, send_buffer, num_bytes);
}

void
serial_runtime::send(int dst, void *buffer, int buffer_size)
{
  spkt_throw(sprockit::illformed_error,
    "serial_runtime::send: should never be called - who would I send to?");
}

void
serial_runtime::recv(int src, void *buffer, int buffer_size)
{
  spkt_throw(sprockit::illformed_error,
    "serial_runtime::recv: should never be called - who would I recv from?");
}

void
serial_runtime::global_sum(long *data, int nelems, int root)
{
  //do nothing
}

void
serial_runtime::global_sum(long long *data, int nelems, int root)
{
  //do nothing
}

void
serial_runtime::global_max(long *data, int nelems, int root)
{
 //do nothing
}

void
serial_runtime::global_max(int *data, int nelems, int root)
{
 //do nothing
}

void
serial_runtime::bcast(void* buffer, int bytes, int root)
{
 //do nothning
}

void
serial_runtime::send_recv_messages(std::vector<incoming_msg>& incoming)
{
 if (incoming.size() != 0){
   spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::send_recv_messages: should not be receiving any messages");
 }
}

void
serial_runtime::send_event(timestamp t, switch_id tid, event* ev)
{
  spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::send_message: should not be sending any messages");
}

void
serial_runtime::wait_merge_array(int tag)
{
}

void
serial_runtime::declare_merge_array(void* buffer, int size, int tag)
{
  merge_refcounts_[tag]++;
}

bool
serial_runtime::release_merge_array(int tag)
{
 int& refcount = merge_refcounts_[tag];
 --refcount;
 if (refcount == 0){
   merge_refcounts_.erase(tag);
   return false;
 }
 else {
   return true;
 }
}

void
serial_runtime::do_send_message(int lp, void* buffer, int size)
{
  spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::do_send_message: should not be sending any messages");
}

void
serial_runtime::do_send_recv_messages(std::vector<void*>& buffers)
{
  spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::do_send_recv_messages: should not be sending any messages");
}

}
}
