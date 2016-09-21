#include <sumi/transport.h>
#include <sumi/thread_safe_set.h>

namespace sumi {

class fake_transport : public transport
{

 public:
  fake_transport(sprockit::sim_parameters* params);

  message::ptr
  block_until_message();

  message::ptr
  block_until_message(double timeout){
    return block_until_message();
  }

  void
  delayed_transport_handle(const message::ptr &msg);

  collective_done_message::ptr
  collective_block(collective::type_t ty, int tag);

  void
  cq_notify();

  void
  schedule_ping_timeout(pinger *pnger, double to);

  void
  schedule_next_heartbeat();

  double
  wall_time() const {
    return 0;
  }

  message::ptr
  pop_rdma_get(){
    return pop_message(rdma_gets_);
  }

  void start_heartbeat(double interval){} //do nothing

  void stop_heartbeat(){} //do nothing

  message::ptr
  pop_rdma_put(){
    return pop_message(rdma_puts_);
  }

  message::ptr
  pop_send(){
    return pop_message(sends_);
  }

  message::ptr
  pop_nvram_get(){
    return pop_message(nvram_gets_);
  }

  void
  simulate_vote(int context, const thread_safe_set<int>& failures);

  void
  allgather(void *dst, void *src, int nelems, int type_size, int tag, bool fault_aware, int context, communicator *dom){} //do nothing

  void
  allreduce(void *dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn, bool fault_aware, int context, communicator *dom){} //do nothing

  void
  dynamic_tree_vote(int vote, int tag, vote_fxn fxn, int context, communicator *dom){}

 private:
  message::ptr
  pop_message(std::list<message::ptr>& msglist);

  void do_smsg_send(int dst, const message::ptr &msg);

  void do_rdma_get(int src, const message::ptr &msg);

  void do_nvram_get(int src, const message::ptr &msg);

  void do_rdma_put(int dst, const message::ptr &msg);

  void do_send_terminate(int dst);

  void do_send_ping_request(int dst);

  void go_die();

  void go_revive();

 private:
  std::list<message::ptr> sends_;

  std::list<message::ptr> rdma_gets_;

  std::list<message::ptr> rdma_puts_;

  std::list<message::ptr> nvram_gets_;
};

}
