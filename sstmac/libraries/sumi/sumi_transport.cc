#include <sstmac/common/sstmac_config.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#endif
#include <sstmac/libraries/sumi/sumi_transport.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/libraries/sumi/message.h>
#include <sumi/message.h>
#include <sprockit/output.h>
#include <sstmac/common/runtime.h>

using namespace sprockit::dbg;

namespace sstmac {

class sumi_server :
  public sstmac::sw::service
{

 public:
  sumi_server(sumi_transport* tport)
    : service(tport->server_libname(),
       sstmac::sw::software_id(-1, -1), //belongs to no application
       tport->os())
  {
  }

  void
  register_proc(int rank, sumi_transport* proc){
    int app_id = proc->sid().app_;
    debug_printf(sprockit::dbg::sumi,
                 "sumi_server registering rank %d for app %d",
                 rank, app_id);
    sumi_transport*& slot = procs_[app_id][rank];
    if (slot){
      spkt_abort_printf("sumi_server: already registered rank %d for app %d on node %d",
                        rank, app_id, os_->addr());
    }
    slot = proc;

    auto iter = pending_.begin();
    auto end = pending_.end();
    while (iter != end){
      auto tmp = iter++;
      transport_message* msg = *tmp;
      if (msg->dest_rank() == rank && msg->dest_app() == proc->sid().app_){
        pending_.erase(tmp);
        proc->incoming_message(msg);
      }
    }
  }

  bool
  unregister_proc(int rank, sumi_transport* proc){
    int app_id = proc->sid().app_;
    auto iter = procs_.find(app_id);
    auto& subMap = iter->second;
    subMap.erase(rank);
    if (subMap.empty()){
      procs_.erase(iter);
    }
    return procs_.empty();
  }

  void
  incoming_event(event *ev){
    transport_message* smsg = safe_cast(transport_message, ev);
    debug_printf(sprockit::dbg::sumi,
                 "sumi_server %d: incoming message %s",
                 os_->addr(), smsg->payload()->to_string().c_str());
    sumi_transport* tport = procs_[smsg->dest_app()][smsg->dest_rank()];
    if (!tport){
      pending_.push_back(smsg);
    } else {
      tport->incoming_message(smsg);
    }
  }

 private:
  std::map<int, std::map<int, sumi_transport*> > procs_;
  std::list<transport_message*> pending_;

};


RegisterAPI("sumi_transport", sumi_transport);

sumi_transport::sumi_transport(sprockit::sim_parameters* params,
               const char* prefix,
               sstmac::sw::software_id sid,
               sstmac::sw::operating_system* os) :
  sumi_transport(params, standard_lib_name(prefix, sid), sid, os)
{
}

sumi_transport::sumi_transport(sprockit::sim_parameters* params,
    const std::string& libname, sstmac::sw::software_id sid,
    sstmac::sw::operating_system* os) :
  //the name of the transport itself should be mapped to a unique name
  api(params, standard_lib_name(libname.c_str(), sid), sid, os),
  //the server is what takes on the specified libname
  server_libname_("sumi_server"),
  process_manager(sid, os),
  transport(params),
  queue_(nullptr)
{
  rank_ = sid.task_;
  library* server_lib = os_->lib(server_libname_);
  sumi_server* server;
  // only do one server per app per node
  if (server_lib == nullptr) {
    server = new sumi_server(this);
    server->start();
  }
  else {
    server = safe_cast(sumi_server, server_lib);
  }

  rank_mapper_ = runtime::launcher()->task_mapper(sid.app_);
  nproc_ = rank_mapper_->nproc();
  loc_ = os_->event_location();

  queue_ = new sumi_queue(os_);
  server->register_proc(rank_, this);
}

void
sumi_transport::ctor_common(sstmac::sw::software_id sid)
{
  rank_ = sid.task_;
  sw::thread* thr = sw::operating_system::current_thread();
  sw::app* my_app = safe_cast(sw::app, thr);
  my_app->compute(timestamp(1e-6));
}

sumi_transport::~sumi_transport()
{
  if (queue_) delete queue_;
  sumi_server* server = safe_cast(sumi_server, os_->lib(server_libname_));
  bool del = server->unregister_proc(rank_, this);
  if (del) delete server;
}

void
sumi_transport::incoming_event(event *ev)
{
  spkt_abort_printf("sumi_transport::incoming_event: should not directly handle events");
}

void
sumi_transport::shutdown_server(int dest_rank, node_id dest_node, int dest_app)
{
  sumi::message::ptr msg = new sumi::system_bcast_message(sumi::system_bcast_message::shutdown, dest_rank);
  client_server_send(dest_rank, dest_node, dest_app, msg);
}

void
sumi_transport::client_server_send(
  int dst_task,
  node_id dst_node,
  int dst_app,
  const sumi::message::ptr& msg)
{
  send(msg->byte_length(),
       dst_task, dst_node, dst_app, msg, false/*no ack*/,
       hw::network_message::payload);
}

void
sumi_transport::client_server_rdma_put(
  int dst_task,
  node_id dst_node,
  int dst_app,
  const sumi::message::ptr& msg)
{
  msg->set_needs_recv_ack(true);
  send(msg->byte_length(),
       dst_task, dst_node, dst_app, msg, false,/*no ack*/
       hw::network_message::rdma_put_payload);
}

sumi::message_ptr
sumi_transport::handle(sstmac::transport_message* smsg)
{
  if (!smsg){
    //this is sloppy - but oh well
    //a null message is sent to me to signal that I have stuff waiting in my completion queue
    debug_printf(sprockit::dbg::sumi, "Rank %d got cq notification", rank_);
    sumi::message::ptr next;
    bool empty = completion_queue_.pop_front_and_return(next);
    if (empty){
      spkt_abort_printf("sumi transport received null message, but completion queue is empty");
    }
    return next;
  }

  sumi::message::ptr my_msg = ptr_safe_cast(sumi::message, smsg->payload());
  debug_printf(sprockit::dbg::sumi,
     "sumi transport rank %d in app %d handling message of type %s: %s",
     rank(), sid().app_, transport_message::tostr(smsg->type()), my_msg->to_string().c_str());
  switch (smsg->type())
  {
   //depending on the type, we might have to mutate the incoming message
   case sstmac::hw::network_message::failure_notification:
    my_msg->set_payload_type(sumi::message::failure);
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
    my_msg->set_payload_type(sumi::message::rdma_get_nack);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::payload_sent_ack:
    my_msg->set_payload_type(sumi::message::eager_payload_ack);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::rdma_get_sent_ack:
    my_msg->set_payload_type(sumi::message::rdma_get_ack);
    transport::handle(my_msg);
    break;
   case sstmac::hw::network_message::rdma_put_sent_ack:
    my_msg->set_payload_type(sumi::message::rdma_put_ack);
    transport::handle(my_msg);
    break;
   default:
    transport::handle(my_msg);
    break; //do nothing
  }

  sumi::message::ptr msg;
  bool empty = completion_queue_.pop_front_and_return(msg);
  return msg;  // will return message::ptr() if empty
}

void
sumi_transport::init()
{
  transport::init();
}

void
sumi_transport::finalize()
{
  debug_printf(sprockit::dbg::sumi, "Rank %d finalizing", rank_);
  transport::finalize();
  monitor_->validate_done();
  stop_heartbeat();
  //sstmac_usleep(heartbeat_interval_*1e6);
  delete monitor_;
  monitor_ = nullptr;
}

void
sumi_transport::send(
  long byte_length,
  const sumi::message_ptr &msg,
  int sendType,
  int dst_rank,
  bool needs_ack)
{
  node_id dst_node = rank_mapper_->node_assignment(dst_rank);
  send(byte_length, dst_rank, dst_node, sid().app_, msg, needs_ack, sendType);
}

void
sumi_transport::send(
  long byte_length,
  int dst_task,
  node_id dst_node,
  int dst_app,
  const sumi::message::ptr& msg,
  bool needs_ack,
  int ty)
{
  sstmac::sw::app_id aid = sid().app_;
  transport_message* tmsg = new transport_message(server_libname_, aid, msg, byte_length);
  tmsg->hw::network_message::set_type((hw::network_message::type_t)ty);
  tmsg->toaddr_ = dst_node;
  tmsg->set_needs_ack(needs_ack);
  tmsg->set_src_rank(rank_);
  tmsg->set_dest_rank(dst_task);
  //send intra-app
  tmsg->set_apps(sid().app_, dst_app);
  sw::library::os_->execute(ami::COMM_SEND, tmsg);
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
sumi_transport::do_smsg_send(int dst, const sumi::message::ptr &msg)
{
  send(msg->byte_length(), msg,
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
  sumi::message::ptr msg = new sumi::message;
  msg->set_class_type(sumi::message::ping);
  //here, a simple rdma get
  rdma_get(dst, msg);
}

void
sumi_transport::do_rdma_get(int dst, const sumi::message::ptr& msg)
{
  send(msg->byte_length(), msg,
    sstmac::hw::network_message::rdma_get_request,
    dst, msg->needs_send_ack());
}

void
sumi_transport::do_rdma_put(int dst, const sumi::message::ptr& msg)
{
  send(msg->byte_length(), msg,
    sstmac::hw::network_message::rdma_put_payload,
    dst, msg->needs_send_ack());
}

void
sumi_transport::do_nvram_get(int dst, const sumi::message::ptr& msg)
{
  send(msg->byte_length(), msg,
    sstmac::hw::network_message::nvram_get_request,
    dst, msg->needs_send_ack());
}

sumi::message::ptr
sumi_transport::block_until_message()
{
  while (1) {
    transport_message* msg = queue_->poll_until_message();
    debug_printf(sprockit::dbg::sumi,
                 "rank %d polling on queue %p returned msg %s",
                 rank(), queue_, msg->to_string().c_str());
    sumi::message_ptr notification = handle(msg);
    delete msg;
    if (notification){
      return notification;
    }
  }
}

sumi::message::ptr
sumi_transport::block_until_message(double timeout)
{
  while (1) {
    transport_message* msg = queue_->poll_until_message(timeout);
    if (msg){
      debug_printf(sprockit::dbg::sumi,
             "block_until_message on rank %d returned payload %s",
             rank(), msg->payload()->to_string().c_str());
      sumi::message_ptr notification = handle(msg);
      delete msg;
      if (notification){
        return notification;
      }
    } else {
      //I timed out
      debug_printf(sprockit::dbg::sumi,
             "block_until_message on rank %d timed out",
             rank());
      return sumi::message_ptr();
    }
  }
}

void
sumi_transport::cq_notify()
{
  debug_printf(sprockit::dbg::sumi, "Rank %d starting cq notification", rank_);
  //a null message indicates a cq notification
  if (blocked()){
    debug_printf(sprockit::dbg::sumi, "Rank %d generating cq notification", rank_);
    incoming_message(nullptr);
  }
}

sumi::collective_done_message::ptr
sumi_transport::collective_block(sumi::collective::type_t ty, int tag)
{
  //first we have to loop through the completion queue to see if it already exists
  while(1)
  {
  std::list<sumi::message::ptr>::iterator it, end = completion_queue_.start_iteration();
  for (it=completion_queue_.begin(); it != end; ++it){
    sumi::message::ptr msg = *it;
    if (msg->class_type() == sumi::message::collective_done){
      //this is a collective done message
      sumi::collective_done_message::ptr cmsg
        = ptr_safe_cast(sumi::collective_done_message, msg);
      if (tag == cmsg->tag() && ty == cmsg->type()){  //done!
        completion_queue_.erase(it);
        completion_queue_.end_iteration();
        return cmsg;
      }
    }
  }

  completion_queue_.end_iteration();
  sumi::message::ptr msg = block_until_message();

  if (msg->class_type() == sumi::message::collective_done){
    //this is a collective done message
    sumi::collective_done_message::ptr cmsg = ptr_safe_cast(sumi::collective_done_message, msg);
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

void
sumi_transport::incoming_message(transport_message *msg)
{
#if SSTMAC_COMM_SYNC_STATS
  if (msg){
    msg->payload()->set_time_arrived(wall_time());
  }
#endif
  queue_->put_message(msg);
}

sumi_queue::sumi_queue(sstmac::sw::operating_system* os)
  : os_(os)
{
}

sumi_queue::sumi_queue() :
  os_(sstmac::sw::operating_system::current_os())
{
}

sumi_queue::~sumi_queue()
{
}

static sstmac::sw::key::category message_thread("Server");

transport_message*
sumi_queue::poll_until_message()
{
  if (pending_messages_.empty()) {
    sstmac::sw::key* blocker = sstmac::sw::key::construct(message_thread);
    blocked_keys_.push_back(blocker);
    debug_printf(sprockit::dbg::sumi,
                 "sumi queue %p has no pending messages - blocking poller %p",
                 this, blocker);
    os_->block(blocker);
    delete blocker;
  }

  //we have been unblocked
  transport_message* msg = pending_messages_.front();
  pending_messages_.pop_front();
  return msg;
}

transport_message*
sumi_queue::poll_until_message(timestamp timeout)
{
  sstmac::sw::key* blocker = 0;
  if (pending_messages_.empty()){
    blocker = sstmac::sw::key::construct(message_thread);
    blocked_keys_.push_back(blocker);
    os_->schedule_timeout(timeout, blocker);
    os_->block(blocker);
    //do not delete - the timeout event will do that
  }

  if (pending_messages_.empty()){
    //timed-out
    //the timed-out blocker is still active in the blocked list - remove it
    std::list<sstmac::sw::key*>::iterator it, end = blocked_keys_.end();
    for (it=blocked_keys_.begin(); it != end; ++it){
        sstmac::sw::key* test = *it;
        if (test == blocker){
          blocked_keys_.erase(it);
          break;
        }
    }
    return NULL;
  } else {
    //we have been unblocked
    transport_message* msg = pending_messages_.front();
    pending_messages_.pop_front();
    return msg;
  }
}

void
sumi_queue::put_message(transport_message* msg)
{
  pending_messages_.push_back(msg);

  if (!blocked_keys_.empty()) {
    sstmac::sw::key* next_key = blocked_keys_.front();
    debug_printf(sprockit::dbg::sumi,
                 "sumi queue %p unblocking poller %p to handle message %s",
                  this, next_key, msg->payload()->to_string().c_str());
    blocked_keys_.pop_front();
    os_->unblock(next_key);
  } else {
    debug_printf(sprockit::dbg::sumi,
                 "sumi queue %p has no pollers to unblock for message %s",
                  this, msg->payload()->to_string().c_str());
  }
}


}
