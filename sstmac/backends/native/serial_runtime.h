#ifndef SERIAL_RUNTIME_H
#define SERIAL_RUNTIME_H

#include <sstmac/backends/common/parallel_runtime.h>

namespace sstmac {
namespace native {

class serial_runtime :
  public parallel_runtime
{
  FactoryRegister("serial", parallel_runtime, serial_runtime)
 public:
  serial_runtime(sprockit::sim_parameters* params);

  int64_t allreduce_min(int64_t mintime) override;

  int64_t allreduce_max(int64_t maxtime) override;

  void global_sum(long *data, int nelems, int root) override;

  void global_sum(long long *data, int nelems, int root) override;

  void global_max(int *data, int nelems, int root) override;

  void global_max(long *data, int nelems, int root) override;

  void gather(void *send_buffer, int num_bytes, void *recv_buffer, int root) override;

  void allgather(void *send_buffer, int num_bytes, void *recv_buffer) override;

  void send(int dst, void *buffer, int buffer_size) override;

  void recv(int src, void *buffer, int buffer_size) override;

  void bcast(void* buffer, int bytes, int root) override;

  void finalize() override {}

  /**
   * @param The topology id to send a remote message to
   * @param buffer The buffer containing a serialized message
   * @param size The size of the buffer being sent
   */
  void send_event(timestamp t, switch_id sid, event* ev);

  void wait_merge_array(int tag) override;

  void declare_merge_array(void* buffer, int size, int tag) override;

  bool release_merge_array(int tag) override;

 protected:
  void do_send_message(int lp, void* buffer, int size) override;

  void do_send_recv_messages(std::vector<void*>& buffers) override;

  std::map<int, int> merge_refcounts_;

};


}
}



#endif // SERIAL_RUNTIME_H
