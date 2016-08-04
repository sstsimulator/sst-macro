#include <sstmac/common/sstmac_config.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#endif
#include <sstmac/libraries/sumi/sumi_transport.h>
#include <sstmac/software/process/app.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/libraries/sumi/message.h>
#include <sumi/message.h>



using namespace sprockit::dbg;

namespace sumi {

RegisterAPI("sumi_transport", sumi_transport);

sumi_transport::sumi_transport(const char* name, sstmac::sw::software_id sid) :
  sumi_api(name, sid)
{
}

sumi::message_ptr
sumi_transport::handle(sstmac::transport_message* smsg)
{
  if (!smsg){
    //this is sloppy - but oh well
    //a null message is sent to me to signal that I have stuff waiting in my completion queue
    bool empty;
    debug_printf(sprockit::dbg::sumi, "Rank %d got cq notification", sumi_api::rank_);
    message::ptr next = completion_queue_.pop_front_and_return(empty);
    if (empty){
      spkt_throw(sprockit::value_error,
        "received null message, but completion queue is empty");
    }
    return next;
  }

  message::ptr my_msg = ptr_safe_cast(message, smsg->payload());
  switch (smsg->type())
  {
   //depending on the type, we might have to mutate the incoming message
   case sstmac::hw::network_message::failure_notification:
    my_msg->set_payload_type(message::failure);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::payload:
   {
    //no work to do - just receive the buffer
    transport::handle(my_msg);
    break;
   }
   case sstmac::hw::network_message::rdma_get_payload:
    my_msg->move_remote_to_local();
    if (my_msg->needs_recv_ack()) //only if I requested to be notified
      transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::rdma_put_payload:
    my_msg->move_local_to_remote();
    if (my_msg->needs_recv_ack()) //only if I requested to be notified
      transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::rdma_get_nack:
    my_msg->set_payload_type(message::rdma_get_nack);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::payload_sent_ack:
    my_msg->set_payload_type(message::eager_payload_ack);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::rdma_get_sent_ack:
    my_msg->set_payload_type(message::rdma_get_ack);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::rdma_put_sent_ack:
    my_msg->set_payload_type(message::rdma_put_ack);
    transport::handle(my_msg);
    break;
   default:
    transport::handle(my_msg);
    break; //do nothing
  }

  bool empty;
  message::ptr msg = completion_queue_.pop_front_and_return(empty);
  return msg;  // will return message::ptr() if empty
}

void
sumi_transport::init()
{
  sstmac::sumi_api::init();
  transport::nproc_ = sstmac::sumi_api::nproc_;
  transport::rank_ = sstmac::sumi_api::rank_;
  transport::init();
}

void
sumi_transport::finalize()
{
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finalizing", transport::rank_);
  sstmac::sumi_api::finalize();
  transport::finalize();
  monitor_->validate_done();
  stop_heartbeat();
  //sstmac_usleep(heartbeat_interval_*1e6);
  delete monitor_;
  monitor_ = nullptr;

}

void
sumi_transport::go_die()
{
  kill_node();
}

void
sumi_transport::go_revive()
{
  spkt_throw(sprockit::illformed_error,
    "SST cannot revive a dead process currently");
}

void
sumi_transport::init_factory_params(sprockit::sim_parameters* params)
{
  sstmac::sumi_api::init_factory_params(params);
  transport::init_factory_params(params);
}

void
sumi_transport::finalize_init()
{
  sstmac::sw::thread::current()->register_lib(this);
  sstmac::sumi_api::finalize_init();
}

void
sumi_transport::do_smsg_send(int dst, const message::ptr &msg)
{
  transport_send(msg->byte_length(), msg,
    sstmac::hw::network_message::payload,
    dst, msg->needs_send_ack());
}

double
sumi_transport::wall_time() const
{
  return now().sec();
}

void
sumi_transport::do_send_ping_request(int dst)
{
  message::ptr msg = new message;
  msg->set_class_type(message::ping);
  //here, a simple rdma get
  rdma_get(dst, msg);
}

void
sumi_transport::do_rdma_get(int dst, const message::ptr& msg)
{
  transport_send(msg->byte_length(), msg,
    sstmac::hw::network_message::rdma_get_request,
    dst, msg->needs_send_ack());
}

void
sumi_transport::do_rdma_put(int dst, const message::ptr& msg)
{
  transport_send(msg->byte_length(), msg,
    sstmac::hw::network_message::rdma_put_payload,
    dst, msg->needs_send_ack());
}

void
sumi_transport::do_nvram_get(int dst, const message::ptr& msg)
{
  transport_send(msg->byte_length(), msg,
    sstmac::hw::network_message::nvram_get_request,
    dst, msg->needs_send_ack());
}

message::ptr
sumi_transport::block_until_message()
{
  sumi::message_ptr msg = sstmac::sumi_api::poll_until_notification();
  return ptr_safe_cast(sumi::message, msg);
}

message::ptr
sumi_transport::block_until_message(double timeout)
{
  sstmac::timestamp to(timeout);
  sumi::message_ptr msg = sstmac::sumi_api::poll_until_notification(to);
  return ptr_test_cast(sumi::message, msg);
}

void
sumi_transport::cq_notify()
{
  debug_printf(sprockit::dbg::sumi, "Rank %d starting cq notification", sumi_api::rank_);
  //a null message indicates a cq notification
  if (blocked()){
    debug_printf(sprockit::dbg::sumi, "Rank %d generating cq notification", sumi_api::rank_);
    incoming_message(NULL);
  }
}

sumi::collective_done_message::ptr
sumi_transport::collective_block(sumi::collective::type_t ty, int tag)
{
  //first we have to loop through the completion queue to see if it already exists
  while(1)
  {
  std::list<message::ptr>::iterator it, end = completion_queue_.start_iteration();
  for (it=completion_queue_.begin(); it != end; ++it){
    message::ptr msg = *it;
    if (msg->class_type() == message::collective_done){
      //this is a collective done message
      collective_done_message::ptr cmsg
        = ptr_safe_cast(collective_done_message, msg);
      if (tag == cmsg->tag() && ty == cmsg->type()){  //done!
        completion_queue_.erase(it);
        completion_queue_.end_iteration();
        return cmsg;
      }
    }
  }

  completion_queue_.end_iteration();
  message::ptr msg = block_until_message();

  if (msg->class_type() == message::collective_done){
    //this is a collective done message
    collective_done_message::ptr cmsg = ptr_safe_cast(collective_done_message, msg);
    if (tag == cmsg->tag() && ty == cmsg->type()){  //done!
      return cmsg;
    }
  }
  completion_queue_.push_back(msg);

  }

}

void
sumi_transport::do_send_terminate(int dst)
{
  //make a no-op

  //spkt_throw(sprockit::unimplemented_error,
  //  "sumi transport for SST should not send terminates");
}

void
sumi_transport::schedule_next_heartbeat()
{
  schedule_delay(heartbeat_interval_,
         new_callback(loc_, this, &sumi_transport::next_heartbeat));
}

void
sumi_transport::delayed_transport_handle(const sumi::message::ptr &msg)
{
  sstmac::callback* done_ev = sstmac::new_callback(
        loc_, this, &transport::handle, msg);
  schedule_delay(sstmac::timestamp(1e-9), done_ev);
}

void
sumi_transport::schedule_ping_timeout(sumi::pinger* pnger, double to)
{
  sstmac::timestamp next_ping_time = api::now() + to;
  sstmac::callback* cb_event = sstmac::new_callback(
        loc_, this, &sumi_transport::ping_timeout, pnger);
  api::schedule(next_ping_time, cb_event);
}

void
sumi_transport::ping_timeout(sumi::pinger* pnger)
{
  pnger->execute();
}


}
