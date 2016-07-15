#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/libraries/unblock_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/libraries/sumi/sumi_api.h>
#include <sstmac/libraries/sumi/message.h>
#include <sprockit/util.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>
#include <sumi/message.h>
#include <sstmac/common/runtime.h>

using namespace sprockit::dbg;

namespace sstmac {

#define print_extra_stuff 0

sumi_api::sumi_api()
{
}

void
sumi_api::init()
{
  rank_mapper_ = runtime::launcher()->task_mapper(sid_.app_);
  nproc_ = rank_mapper_->nproc();
  loc_ = os_->event_location();

  library* server_lib = os_->lib(server_libname_);
  sumi_server* server;

  // only do one server per app per node
  if (server_lib == 0) {
    server = new sumi_server(sid_.app_);
    register_lib(server);
    server->start();
  }
  else {
    server = safe_cast(sumi_server, server_lib);
  }

  server->register_proc(rank_, this);

  queue_ = new sumi_queue(os_);
}

void
sumi_api::init_os(sw::operating_system* os)
{
  process_manager::init_os(os);
  api::init_os(os);
}

void
sumi_api::finalize()
{
  unregister_all_libs();
}

void
sumi_api::init_param1(const sstmac::sw::software_id &sid)
{
  sid_ = sid;
  rank_ = sid.task_;
  libname_ = sprockit::printf("sumi_api_%d_%d",
                       int(sid.app_), int(sid.task_));
  server_libname_ = sprockit::printf("sumi_server_%d", int(sid_.app_));
  process_manager::init_param1(sid);
  sw::thread* thr = sw::operating_system::current_thread();
  sw::app* my_app = safe_cast(sw::app, thr);
  my_app->compute(timestamp(1e-6));
}

void
sumi_api::init_factory_params(sprockit::sim_parameters *params)
{
  api::init_factory_params(params);
}

void
sumi_api::transport_send(
  long byte_length,
  const sumi::message_ptr &msg,
  int sendType,
  int dst,
  bool needs_ack,
  void* buffer)
{
  sstmac::sw::app_id aid = sid_.app_;
  sstmac::hw::network_message::type_t ty = (sstmac::hw::network_message::type_t) sendType;
  transport_message* tmsg = new transport_message(aid, msg, byte_length);
  tmsg->hw::network_message::set_type(ty);
  tmsg->set_lib_name(server_libname_);
  tmsg->toaddr_ = rank_mapper_->node_assignment(sw::task_id(dst));
  tmsg->set_needs_ack(needs_ack);
  tmsg->set_src(rank_);
  tmsg->set_dest(dst);
  tmsg->set_buffer(buffer);

  sw::library::os_->execute_kernel(ami::COMM_SEND, tmsg);
}

sumi::message_ptr
sumi_api::poll_until_notification()
{
#if SSTMAC_SANITY_CHECK
  if (!queue_){
    spkt_throw(sprockit::null_error,
        "sstmasg_api::poll: api not yet initialized");
  }
#endif
  while (1) {
    transport_message* msg = queue_->poll_until_message();
    sumi::message_ptr notification = handle(msg);
    if (notification){
      return notification;
    }
  }
}

sumi::message_ptr
sumi_api::poll_until_notification(timestamp timeout)
{
  while (1) {
    transport_message* msg = queue_->poll_until_message(timeout);
    if (msg){
      sumi::message_ptr notification = handle(msg);
      if (notification){
        return notification;
      }
    } else {
      //I timed out
      return sumi::message_ptr();
    }
  }
}

void
sumi_api::incoming_message(transport_message* msg)
{
  queue_->put_message(msg);
}

sumi_server::sumi_server(int appid)
  : appid_(appid)
{
  libname_ = sprockit::printf("sumi_server_%d", appid);
}

void
sumi_server::incoming_event(event* ev)
{
 transport_message* smsg = safe_cast(transport_message, ev);
 try {
  get_proc(smsg->dest())->incoming_message(smsg);
 } catch (sprockit::value_error& e) {
   cerrn << "sumi_server::handle: failed handling "
     << ev->to_string() << std::endl;
   throw e;
 }

}


sumi_api*
sumi_server::get_proc(int rank) const
{
  spkt_unordered_map<int, sumi_api*>::const_iterator
  it = procs_.find(rank);
  if (it == procs_.end()) {
    it = procs_.begin();
    cerrn << "Valid ranks for server are:\n";
    while (it != procs_.end()) {
      cerrn << it->first << std::endl;
      ++it;
    }

    spkt_throw_printf(sprockit::value_error,
       "sumi_server::get_proc: invalid rank %d for server", rank);
  }
  return it->second;
}

void
sumi_server::register_proc(int rank, sumi_api* proc)
{
  procs_[rank] = proc;
}


sumi_queue::sumi_queue(sstmac::sw::operating_system* os)
  : os_(os)
{
}

sumi_queue::sumi_queue() :
  os_(sstmac::sw::operating_system::current_os())
{
}

static sstmac::sw::key::category message_thread("Server");

transport_message*
sumi_queue::poll_until_message()
{
  if (pending_messages_.empty()) {
    sstmac::sw::key* blocker = sstmac::sw::key::construct(message_thread);
    blocked_keys_.push_back(blocker);
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
    blocked_keys_.pop_front();
    os_->unblock(next_key);
  }
}


}

