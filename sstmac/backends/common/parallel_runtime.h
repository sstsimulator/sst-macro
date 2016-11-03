#ifndef PARALLEL_RUNTIME_H
#define PARALLEL_RUNTIME_H


#include <sstmac/common/messages/message_buffer_cache.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_location.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/backends/common/sim_partition_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters.h>

DeclareDebugSlot(parallel);

namespace sstmac {

class parallel_runtime :
  public lockable
{
 public:
  virtual ~parallel_runtime();

  struct incoming_msg {
    switch_id sid;
    void* buffer;
  };

  static const int global_root;

  virtual int64_t
  allreduce_min(int64_t mintime) = 0;

  virtual int64_t
  allreduce_max(int64_t maxtime) = 0;

  virtual void
  global_sum(long* data, int nelems, int root) = 0;

  virtual void
  global_sum(long long* data, int nelems, int root) = 0;

  virtual void
  global_max(int* data, int nelems, int root) = 0;

  virtual void
  global_max(long* data, int nelems, int root) = 0;

  virtual void
  send(int dst, void* buffer, int buffer_size) = 0;

  virtual void
  gather(void* send_buffer, int num_bytes, void* recv_buffer, int root) = 0;

  virtual void
  allgather(void* send_buffer, int num_bytes, void* recv_buffer) = 0;

  virtual void
  recv(int src, void* buffer, int buffer_size) = 0;

  int
  global_max(int my_elem){
    int dummy = my_elem;
    global_max(&dummy, 1, global_root);
    return dummy;
  }

  long
  global_max(long my_elem){
    long dummy = my_elem;
    global_max(&dummy, 1, global_root);
    return dummy;
  }

  virtual void
  bcast(void* buffer, int bytes, int root) = 0;

  void
  bcast_string(std::string& str, int root);

  std::istream*
  bcast_file_stream(const std::string& fname);

  virtual void
  finalize() = 0;

  virtual void
  init_runtime_params(sprockit::sim_parameters* params);

  virtual void
  init_partition_params(sprockit::sim_parameters* params);

  /**
   @param pool A buffer cache corresponding to a pool of free buffers
   @param incoming A buffer cache holding buffers that correspond to incoming messages
  */
  virtual void
  send_recv_messages(std::vector<void*>& incoming);

  /**
   * @param The topology id to send a remote message to
   * @param buffer The buffer containing a serialized message
   * @param size The size of the buffer being sent
   */
  virtual void
  send_event(int thread_id,
    timestamp t,
    device_id tid,
    device_id src,
    uint32_t seqnum,
    event* ev);

  int
  me() const {
    return me_;
  }

  int
  nproc() const {
    return nproc_;
  }

  int
  nthread() const {
    return nthread_;
  }

  int
  ser_buf_size() const {
    return buf_size_;
  }

  partition*
  topology_partition() const {
    return part_;
  }

  virtual void
  wait_merge_array(int tag) = 0;

  virtual void
  declare_merge_array(void* buffer, int size, int tag) = 0;

  virtual bool
  release_merge_array(int tag) = 0;

  void
  free_recv_buffers(const std::vector<void*>& buffers);

  static parallel_runtime*
  static_runtime(sprockit::sim_parameters* params);

  static void
  clear_static_runtime(){
    if (static_runtime_) delete static_runtime_;
    static_runtime_ = nullptr;
  }

 protected:
  parallel_runtime(sprockit::sim_parameters* params,
                   int me, int nproc);

  virtual void
  do_send_message(int lp, void* buffer, int size) = 0;

  virtual void
  do_send_recv_messages(std::vector<void*>& buffers) = 0;

 protected:
   int nproc_;
   int nthread_;
   int me_;
   std::vector<message_buffer_cache> send_buffer_pools_;
   message_buffer_cache recv_buffer_pool_;
   typedef std::pair<int, void*> send_buf_t;
   std::vector<std::vector<void*> > send_buffers_;
   int buf_size_;
   partition* part_;
   static parallel_runtime* static_runtime_;

};

DeclareFactory(parallel_runtime);

}

#endif // PARALLEL_RUNTIME_H
