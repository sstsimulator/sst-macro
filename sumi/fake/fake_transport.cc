#include <fake/fake_transport.h>
#include <sprockit/sim_parameters.h>
#include <unistd.h>

namespace sumi {

SpktRegister("fake", transport, fake_transport, "A fake transport that doesn't actually send messages - just logs them");

static message::ptr fake_msg;

message::ptr
fake_transport::block_until_message()
{
  sleep(1);
  if (!fake_msg){
    fake_msg = new message;
    fake_msg->set_class_type(message::fake);
  }
  return fake_msg;
}

collective_done_message::ptr
fake_transport::collective_block(collective::type_t ty, int tag)
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never block");
}

void
fake_transport::delayed_transport_handle(const message::ptr &msg)
{
  handle(msg);
}

void
fake_transport::cq_notify()
{
  //do nothng
}

void
fake_transport::schedule_ping_timeout(pinger *pnger, double to)
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never ping or have failed pings");
}

void
fake_transport::schedule_next_heartbeat()
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never ping or have failed pings");
}

void
fake_transport::do_smsg_send(int dst, const message::ptr &msg)
{
  sends_.push_back(msg);
}

fake_transport::fake_transport(sprockit::sim_parameters *params) :
  transport(params)
{
  nproc_ = params->get_int_param("fake_transport_nproc");
  rank_ = params->get_int_param("fake_transport_rank");
}

void
fake_transport::do_rdma_get(int src, const message::ptr &msg)
{
  rdma_gets_.push_back(msg);
}

void
fake_transport::do_rdma_put(int dst, const message::ptr &msg)
{
  rdma_puts_.push_back(msg);
}

void
fake_transport::do_nvram_get(int src, const message::ptr &msg)
{
  nvram_gets_.push_back(msg);
}

void
fake_transport::simulate_vote(int context, const thread_safe_set<int> &failures)
{
  votes_done_[context] = vote_result(1, failures);
}

message::ptr
fake_transport::pop_message(std::list<message::ptr> &msglist)
{
  if (msglist.empty()){
    return message::ptr();
  } else {
    message::ptr ret = msglist.front();
    msglist.pop_front();
    return ret;
  }
}

void
fake_transport::do_send_terminate(int dst)
{
  spkt_throw(sprockit::unimplemented_error,
    "fake transport should not send terminates");
}

void
fake_transport::do_send_ping_request(int dst)
{
  spkt_throw(sprockit::unimplemented_error, "fake transport should never ping or have failed pings");
}

void
fake_transport::go_die()
{
  spkt_throw(sprockit::unimplemented_error,
    "fake transport should never die");
}

void
fake_transport::go_revive()
{
  spkt_throw(sprockit::unimplemented_error,
    "fake transport should go revive");
}

}
