#ifndef SERIAL_RUNTIME_H
#define SERIAL_RUNTIME_H

#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {
namespace native {

class serial_runtime :
  public parallel_runtime
{
 public:
  serial_runtime();

  std::string
  to_string() const {
    return "serial runtime";
  }

  virtual int64_t
  allreduce_min(int64_t mintime);

  virtual int64_t
  allreduce_max(int64_t maxtime);

  virtual void
  global_sum(long *data, int nelems, int root);

  virtual void
  global_sum(long long *data, int nelems, int root);

  virtual void
  global_max(int *data, int nelems, int root);

  virtual void
  global_max(long *data, int nelems, int root);

  virtual void
  gather(void *send_buffer, int num_bytes, void *recv_buffer, int root);

  virtual void
  allgather(void *send_buffer, int num_bytes, void *recv_buffer);

  virtual void
  send(int dst, void *buffer, int buffer_size);

  virtual void
  recv(int src, void *buffer, int buffer_size);

  virtual void
  bcast(void* buffer, int bytes, int root);

  void finalize(){}

  /**
   @param pool A buffer cache corresponding to a pool of free buffers
   @param incoming A buffer cache holding buffers that correspond to incoming messages
  */
  void
  send_recv_messages(std::vector<incoming_msg>& incoming);

  /**
   * @param The topology id to send a remote message to
   * @param buffer The buffer containing a serialized message
   * @param size The size of the buffer being sent
   */
  void
  send_message(timestamp t, topology_id tid, sst_message* msg);

  virtual void
  wait_merge_array(int tag);

  virtual void
  declare_merge_array(void* buffer, int size, int tag);

  virtual bool
  release_merge_array(int tag);

 protected:
  virtual void
  do_send_message(int lp, void* buffer, int size);

  virtual void
  do_send_recv_messages(std::vector<void*>& buffers);

  std::map<int, int> merge_refcounts_;

};

DeclareFactory(parallel_runtime);


}
}



#endif // SERIAL_RUNTIME_H
