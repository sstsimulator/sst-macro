#include <sumi/collective_actor.h>
#include <sumi/partner_timeout.h>
#include <sumi/transport.h>
#include <sumi/ping.h>
#include <sumi/domain.h>
#include <sprockit/output.h>
#include <cstring>
#include <utility>

/*
#undef debug_printf
#define debug_printf(flags, ...) \
  if (tag_ == 221) std::cout << sprockit::spkt_printf(__VA_ARGS__) << std::endl
*/

RegisterDebugSlot(sumi_collective_buffer)

namespace sumi
{

using namespace sprockit::dbg;

std::string
action::to_string() const
{
  return sprockit::printf("action %s r=%d,p=%d,o=%d,n=%d,t=%s",
     action::tostr(type),
     round, partner, offset, nelems,
     action::tostr(recv_type));
}

void
debug_print(const char* info, const std::string& rank_str,
            int partner, int round, int offset, int nelems,
            int type_size, const void* buffer)
{
  if (!buffer) return;

  std::cout << sprockit::printf("Rank %s, partner %d, round=%d, %s %p [%2d:%2d] = { ",
      rank_str.c_str(), partner, round, info, buffer, offset, offset + nelems);
  char* tmp = (char*) buffer;
  for (int i=0; i < nelems; ++i, tmp += type_size){
    int elem;
    if (type_size == sizeof(int)){
      int* elemPtr = (int*) tmp;
      elem = *elemPtr;
    }
    else if (type_size == sizeof(double)){
      double* elemPtr = (double*) tmp;
      elem = *elemPtr;
    } else {
      int* elemPtr = (int*) tmp;
      elem = *elemPtr;
    }

    std::cout << sprockit::printf("%2d ", elem);
  }
  std::cout << " }\n";
}

void
collective_actor::init(transport *my_api, domain *dom, int tag, int context, bool fault_aware)
{
  rank_map_.init(my_api->failed_ranks(context), dom);
  tag_ = tag;
  dom_ = dom;
  my_api_ = my_api;
  complete_ = false;
  fault_aware_ = fault_aware;
  dense_nproc_ = rank_map_.dense_rank(dom->nproc());
  dense_me_ = rank_map_.dense_rank(dom->my_domain_rank());
}

collective_actor::collective_actor(
    transport* my_api, domain* dom,
    int tag, int context, bool fault_aware)
{
  init(my_api, dom, tag, context, fault_aware);
  timeout_ = new collective_timeout(this);
}

collective_actor::~collective_actor()
{
  delete timeout_;
}

std::string
collective_actor::rank_str(int dense_rank) const
{
  int dom_rank = domain_rank(dense_rank);
  int global_rank = dom_->domain_to_global_rank(dom_rank);
  return sprockit::printf("%d=%d:%d",
    global_rank, dense_rank, dom_rank);
}

std::string
collective_actor::rank_str() const
{
  return sprockit::printf("%d=%d:%d",
    my_api_->rank(), dense_me_, domain_rank(dense_me_));
}

void
collective_actor::partner_ping_failed(int global_rank)
{
  //map this to virtual rank
  int domain_rank = dom_->global_to_domain_rank(global_rank);
  int dense_rank = rank_map_.dense_rank(domain_rank);
  dense_partner_ping_failed(dense_rank);
}

void
collective_actor::cancel_ping(int dense_rank)
{
  if (!fault_aware_)
    return;

  int dom_rank = domain_rank(dense_rank);
  if (dom_rank == dom_->my_domain_rank())  //no need to ping self
    return;

  std::map<int,int>::iterator it = ping_refcounts_.find(dom_rank);
  if (it == ping_refcounts_.end())
    spkt_throw_printf(sprockit::illformed_error,
        "dag_collective_actor trying to cancel non-existent ping");

  int& refcount = it->second;
  --refcount;
  if (refcount == 0){
    int global_phys_rank = dom_->domain_to_global_rank(dom_rank);
    debug_printf(sumi_collective | sumi_ping,
      "Rank %s collective %s(%p) erase ping for partner %d:%d:%d on tag=%d ",
      rank_str().c_str(), to_string().c_str(), this,
      dense_rank, dom_rank, global_phys_rank, tag_);
      ping_refcounts_.erase(it);
      stop_check_neighbor(global_phys_rank);
  }
  else {
    debug_printf(sumi_collective | sumi_ping,
        "Rank %s collective %s(%p) decrement ping refcount to %d for partner %d:%d on tag=%d ",
    rank_str().c_str(), to_string().c_str(), this,
    refcount,
    dense_rank, dom_rank, tag_);
  }
}

bool
collective_actor::check_neighbor(int global_phys_rank)
{
  return my_api_->start_watching(global_phys_rank, timeout_);
}

void
collective_actor::stop_check_neighbor(int global_phys_rank)
{
  my_api_->stop_watching(global_phys_rank, timeout_);
}

bool
collective_actor::ping_domain_rank(int domain_rank, int dense_rank)
{
  int& refcount = ping_refcounts_[domain_rank];
  if (refcount != 0){
     debug_printf(sumi_collective | sumi_ping,
         "Rank %s collective %s(%p) already pinging %d with refcount=%d on tag=%d ",
         rank_str().c_str(), to_string().c_str(), this,
         domain_rank, refcount, tag_);
    //we be pinging in the rain, just pinging in the rain
    ++refcount;
    return false; //all is well, we think - we have a pending ping
  }
  else {
    int global_phys_rank = dom_->domain_to_global_rank(domain_rank);
    debug_printf(sumi_collective | sumi_ping,
      "Rank %s collective %s(%p) begin pinging %d:%d on tag=%d ",
      rank_str().c_str(), to_string().c_str(), this,
      domain_rank, global_phys_rank, tag_);

    //we don't know anything - do a more extensive check
    bool is_dead = check_neighbor(global_phys_rank);
    if (is_dead){
      debug_printf(sumi_collective | sumi_ping,
        "Rank %s collective %s(%p) sees that %d:%d is apparently dead on tag=%d ",
        rank_str().c_str(), to_string().c_str(), this,
        domain_rank, global_phys_rank, tag_);
      failed_ranks_.insert(dense_rank);
      ping_refcounts_.erase(domain_rank);
      return true;
    }
    else {
      debug_printf(sumi_collective | sumi_ping,
        "Rank %s collective %s(%p) has started new ping to %s on tag=%d ",
        rank_str().c_str(), to_string().c_str(), this,
        rank_str(dense_rank).c_str(),
        tag_);
      ++refcount;
      return false; //nope, all good
    }
  }
}

bool
collective_actor::do_ping_neighbor(int dense_rank)
{
  int dom_rank = domain_rank(dense_rank);
  if (dom_rank == dom_->my_domain_rank()){
    //no reason to ping self
    return false;
  }
  return ping_domain_rank(dom_rank, dense_rank);
}

bool
collective_actor::ping_neighbor(int dense_rank)
{
  if (!fault_aware_){
    return false; //not failed
  }
  else if (is_failed(dense_rank)){
    //this guy is failed - no reason to communicate
    return true;
  } else {
    return do_ping_neighbor(dense_rank);
  }
}

int
collective_actor::global_rank(int dense_rank) const
{
  return dom_->domain_to_global_rank(domain_rank(dense_rank));
}

int
collective_actor::domain_rank(int dense_rank) const
{
  return rank_map_.sparse_rank(dense_rank);
}

int
collective_actor::dense_to_global_dst(int dense_dst)
{
  int domain_dst = domain_rank(dense_dst);
  int global_physical_dst = dom_->domain_to_global_rank(domain_dst);
  debug_printf(sumi_collective |  sumi_collective_sendrecv,
    "Rank %s sending message to %s on tag=%d, domain=%d, physical=%d",
    rank_str().c_str(),
    rank_str(dense_dst).c_str(),
    tag_, domain_dst, global_physical_dst);
  return global_physical_dst;
}

void
collective_actor::send_payload(int dense_dst, const message::ptr& msg)
{
  int dst = dense_to_global_dst(dense_dst);
  my_api_->smsg_send(dst, message::eager_payload, msg);
}

void
collective_actor::send_header(int dense_dst, const message::ptr& msg)
{
  int dst = dense_to_global_dst(dense_dst);
  my_api_->smsg_send(dst, message::header, msg);
}

void
collective_actor::rdma_put(int dense_dst, const message::ptr& msg)
{
  int domain_dst = domain_rank(dense_dst);
  int global_physical_dst = dom_->domain_to_global_rank(domain_dst);
  debug_printf(sumi_collective | sumi_collective_sendrecv,
    "Rank %s, RDMA put message to %s on tag=%d ",
    rank_str().c_str(),
    rank_str(dense_dst).c_str(),
    tag_);

  my_api_->rdma_put(global_physical_dst, msg,
    true/*need a send ack*/,
    true/*need a remote recv ack*/);
}

void
collective_actor::rdma_get(int dense_dst, const message::ptr& msg)
{
  int domain_dst = domain_rank(dense_dst);
  int global_physical_dst = dom_->domain_to_global_rank(domain_dst);
  debug_printf(sumi_collective | sumi_collective_sendrecv,
    "Rank %s, RDMA get message from %s on tag=%d ",
    rank_str().c_str(),
    rank_str(dense_dst).c_str(),
    tag_);
  my_api_->rdma_get(global_physical_dst, msg,
    true/*need a send ack*/,
    true/*need a local recv ack*/);
}

void
dag_collective_actor::fail_actions(int dense_dst)
{
  active_map tmp = active_comms_;
  active_map::iterator it, end = tmp.end();
  for (it=tmp.begin(); it != end; ++it){
    action* ac = it->second;
    if (ac->partner == dense_dst){
      action_done(ac);
    }
  }
}

void
dag_collective_actor::dense_partner_ping_failed(int dense_rank)
{
  debug_printf(sumi_collective,
     "Rank %s on collective %s - partner %s returned failed ping",
      rank_str().c_str(), collective::tostr(type_),
      rank_str(dense_rank).c_str());
  failed_ranks_.insert(dense_rank);
  fail_actions(dense_rank);
}

void
collective_actor::validate_pings_cleared()
{
  int size = ping_refcounts_.size();
  if (size){
    spkt_throw_printf(sprockit::illformed_error,
        "dag_collective_actor::rank %d still has %d outstanding pings\n",
        my_api_->rank(), ping_refcounts_.size());
  }
}

void
dag_collective_actor::start()
{
  std::list<action*>::iterator tmp,
      it = initial_actions_.begin(),
      end = initial_actions_.end();
  while (it != end){
    tmp = it++;
    action* ac = *tmp;
    initial_actions_.erase(tmp);
    start_action(ac);
  }
}

void
dag_collective_actor::start_action(action* ac)
{
  debug_printf(sumi_collective,
   "Rank %s starting action %s to partner %s on round %d offset %d -> id = %u: %d pending send headers, %d pending recv headers",
    rank_str().c_str(), action::tostr(ac->type),
    rank_str(ac->partner).c_str(), ac->round, ac->offset,
    ac->id,
    pending_send_headers_.size(),
    pending_recv_headers_.size());
  switch (ac->type){
    case action::send:
      start_send(ac);
      break;
    case action::unroll:
      break;
    case action::shuffle:
      do_sumi_debug_print("recv buf before shuffle",
        rank_str().c_str(), ac->partner,
        ac->round,
        0, ac->nelems, type_size_,
        recv_buffer_.ptr);
      do_sumi_debug_print("result buf before shuffle",
        rank_str().c_str(), ac->partner,
        ac->round,
        0, nelems_*dense_nproc_, type_size_,
        result_buffer_.ptr);
      start_shuffle(ac);
      do_sumi_debug_print("result buf after shuffle",
        rank_str().c_str(), ac->partner,
        ac->round,
        0, nelems_*dense_nproc_, type_size_,
        result_buffer_.ptr);
      do_sumi_debug_print("send buf after shuffle",
        rank_str().c_str(), ac->partner,
        ac->round,
        0, ac->nelems, type_size_,
        send_buffer_.ptr);
      clear_action(ac);
      break;
    case action::recv:
      start_recv(ac);
      break;
  }
}

void
dag_collective_actor::start_shuffle(action *ac)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "collective %s does not shuffle data - invalid DAG",
    collective::tostr(type_));
}

void
dag_collective_actor::clear_action(action* ac)
{
  active_comms_.erase(ac->id);
  check_collective_done();

  std::multimap<uint32_t, action*>::iterator it = pending_comms_.find(ac->id);
  std::list<action*> pending_actions;
  while (it != pending_comms_.end()){
    action* pending = it->second;
    pending_actions.push_back(pending);
    pending_comms_.erase(it);

    pending->join_counter--;
    debug_printf(sumi_collective,
      "Rank %s satisfying dependency to join counter %d for action %s to partner %s on round %d"
      " with action %s from partner %s on round %d tag=%d",
      rank_str().c_str(), pending->join_counter,
      action::tostr(pending->type),
      rank_str(pending->partner).c_str(), pending->round,
      action::tostr(ac->type),
      rank_str(ac->partner).c_str(), ac->round,
      tag_);

    if (pending->join_counter == 0){
      start_action(pending);
    }

    it = pending_comms_.find(ac->id);
  }

  completed_actions_.push_back(ac);
}

void
dag_collective_actor::action_done(action* ac)
{
  debug_printf(sumi_collective,
    "Rank %s finishing action %s to partner %s on round %d -> id %u tag=%d",
    rank_str().c_str(), action::tostr(ac->type),
    rank_str(ac->partner).c_str(), ac->round, ac->id,
    tag_);

  cancel_ping(ac->partner);
  clear_action(ac);
}

void
dag_collective_actor::send_eager_message(action* ac)
{
  collective_eager_message::ptr msg
    = new collective_eager_message(type_, collective_work_message::eager_payload,
      send_buffer(ac->offset), ac->nelems, type_size_, tag_,
      ac->round, dense_me_, ac->partner);

  debug_printf(sumi_collective | sumi_collective_sendrecv | sumi_failure,
   "Rank %s, collective %s(%p) sending eager message to %d on tag=%d "
   "for buffer %p = %d + %p",
   rank_str().c_str(), to_string().c_str(), this,
   ac->partner, tag_,
   msg->eager_buffer(),
   ac->offset, send_buffer_.ptr);

  do_sumi_debug_print("sending to", rank_str().c_str(), ac->partner,
   ac->round, ac->offset, msg->nelems(), type_size_, msg->eager_buffer());

  send_payload(ac->partner, msg);
  action_done(ac);
}

void
dag_collective_actor::send_rdma_put_header(action* ac)
{
  collective_rdma_message::ptr msg
    = new collective_rdma_message(type_,
      collective_work_message::rdma_put_header,
      ac->nelems, type_size_, tag_,
      ac->round,
      dense_me_,
      ac->partner);
  msg->remote_buffer() = recv_buffer(ac);

  debug_printf(sumi_collective | sumi_collective_sendrecv | sumi_failure,
   "Rank %s, collective %s(%p) sending put header %p to %s on round=%d tag=%d "
   "for buffer %p = %d + %p",
   rank_str().c_str(), to_string().c_str(), this,
   msg.get(),
   rank_str(ac->partner).c_str(),
   ac->round, tag_,
   msg->remote_buffer().ptr, ac->offset, recv_buffer_.ptr);

  int dst = dense_to_global_dst(ac->partner);
  my_api_->send_rdma_header(dst, msg);
}

void
dag_collective_actor::send_rdma_get_header(action* ac)
{
  collective_rdma_message::ptr msg = new collective_rdma_message(
    type_, collective_work_message::rdma_get_header,
       ac->nelems, type_size_, tag_,
       ac->round, dense_me_,
       ac->partner);
  msg->remote_buffer() = send_buffer(ac->offset);


  debug_printf(sumi_collective | sumi_collective_sendrecv | sumi_failure,
   "Rank %s, collective %s(%p) sending rdma get message to %s on round=%d tag=%d "
   "for buffer %p = %d + %p",
   rank_str().c_str(), to_string().c_str(), this,
   rank_str(ac->partner).c_str(),
   ac->round, tag_,
   msg->remote_buffer().ptr,
   ac->offset, send_buffer_.ptr);

  int dst = dense_to_global_dst(ac->partner);
  my_api_->send_rdma_header(dst, msg);
}

void
dag_collective_actor::add_initial_action(action* ac)
{
  debug_printf(sumi_collective | sumi_collective_init,
   "Rank %s, collective %s adding initial %s on tag=%d",
   rank_str().c_str(), collective::tostr(type_), 
   ac->to_string().c_str(), tag_);
  initial_actions_.push_back(ac);
}

void
dag_collective_actor::add_dependency(action* precursor, action *ac)
{
  if (precursor){
    debug_printf(sumi_collective | sumi_collective_init,
     "Rank %s, collective %s adding dependency %s to %s tag=%d",
     rank_str().c_str(), collective::tostr(type_),
     precursor->to_string().c_str(),
     ac->to_string().c_str(), tag_);
    pending_comms_.insert(std::make_pair(precursor->id, ac));
    ac->join_counter++;
  } else {
    add_initial_action(ac);
  }
}

dag_collective_actor::~dag_collective_actor()
{
  std::list<action*>::iterator it, end = completed_actions_.end();
  for (it=completed_actions_.begin(); it != end; ++it){
    action* ac = *it;
    delete ac;
  }
  completed_actions_.clear();
}

void
dag_collective_actor::check_collective_done()
{
  debug_printf(sumi_collective,
      "Rank %s has %d active comms, %d pending comms, %d initial comms",
      rank_str().c_str(), active_comms_.size(), pending_comms_.size(), initial_actions_.size());
  if (active_comms_.empty() && pending_comms_.empty() && initial_actions_.empty()){
    finalize();
    put_done_notification();
  }
}

void
dag_collective_actor::start_send(action* ac)
{
  if (failed()){
    start_send_nack_instead(ac);
    clear_action(ac);
    return;
  }

  bool send_failed = ping_neighbor(ac->partner);
  if (send_failed){
    erase_pending(ac->id, pending_send_headers_);
    clear_action(ac);
  } else {
    active_comms_[ac->id] = ac;
    reput_pending(ac->id, pending_send_headers_);
    do_send(ac);
  }
}

void
dag_collective_actor::do_send(action* ac)
{
  protocol_t pr = protocol_for_action(ac);
  switch(pr){
    case eager_protocol:
      send_eager_message(ac);
      break;
    case get_protocol:
     send_rdma_get_header(ac);
     break;
    case put_protocol:
      break; //do nothing for put
  }
}

dag_collective_actor::protocol_t
dag_collective_actor::protocol_for_action(action* ac) const
{
  long byte_length = ac->nelems*type_size_;
  if (my_api_->use_eager_protocol(byte_length)){
    return eager_protocol;
  } else if (my_api_->use_get_protocol()){
    return get_protocol;
  } else {
    return put_protocol;
  }
}

void
dag_collective_actor::start_send_nack_instead(action* ac)
{
  debug_printf(sumi_collective,
    "Rank %s nacking send to partner %s on round %d",
    rank_str().c_str(), rank_str(ac->partner).c_str(), ac->round);

  protocol_t pr = protocol_for_action(ac);
  switch(pr){
    case eager_protocol:
      send_failure_message(ac, collective_work_message::nack_eager);
      break;
    case get_protocol:
      send_failure_message(ac, collective_work_message::nack_get_header);
      break;
    case put_protocol:
      break;
  }
}

void
dag_collective_actor::start_recv(action* ac)
{
  //if (failed()){
  //  start_recv_nack_instead(ac);
  //  clear_action(ac, active_recvs_);
  //  return;
  //}

  //I'm not allow
  bool recv_failed = ping_neighbor(ac->partner);
  if (recv_failed){
    debug_printf(sumi_collective,
      "Rank %s started to recv from partner %s on round %d, but partner failed",
      rank_str().c_str(), rank_str(ac->partner).c_str(), ac->round);
    erase_pending(ac->id, pending_recv_headers_);
    clear_action(ac);
  } else {
    do_recv(ac);
    reput_pending(ac->id, pending_recv_headers_);
  }
}

void
dag_collective_actor::do_recv(action* ac)
{
  active_comms_[ac->id] = ac;
  long byte_length = ac->nelems*type_size_;
  if (my_api_->use_eager_protocol(byte_length) || my_api_->use_get_protocol()){
    //I need to wait for the sender to contact me
  } else {
    if (failed()){
      spkt_throw(sprockit::unimplemented_error,
         "dag_collective_actor: cannot handle failures with put protocol");
    }
    //put protocol, I need to tell the sender where to put it
    send_rdma_put_header(ac);
  }

}

void
dag_collective_actor::deadlock_check() const
{
  std::cout << sprockit::printf("  deadlocked actor %d of %d on tag %d",
    dense_me_, dense_nproc_, tag_) << std::endl;

  {std::list<action*>::const_iterator it, end = completed_actions_.end();
  for (it=completed_actions_.begin(); it != end; ++it){
    action* ac = *it;
    std::cout << sprockit::printf("    Rank %s: completed action %s partner %d round %d",
                      rank_str().c_str(), action::tostr(ac->type), ac->partner, ac->round) << std::endl;
  }}

  {active_map::const_iterator it, end = active_comms_.end();
  for (it=active_comms_.begin(); it != end; ++it){
    action* ac = it->second;
    std::cout << sprockit::printf("    Rank %s: active %s",
                    rank_str().c_str(), ac->to_string().c_str()) << std::endl;
  }}

  {std::pair<pending_map::const_iterator, pending_map::const_iterator> range;
  pending_map::const_iterator it, end = pending_comms_.end();
  for (it=pending_comms_.begin(); it != end; ++it){
    uint32_t id = it->first;
    action::type_t ty;
    int r, p;
    action::details(id, ty, r, p);
    range = pending_comms_.equal_range(id);
    if (range.first != range.second){
      std::cout << sprockit::printf("    Rank %s: waiting on action %s partner %d round %d",
                      rank_str().c_str(), action::tostr(ty), p, r) << std::endl;
    }

    for (pending_map::const_iterator rit=range.first; rit != range.second; ++rit){
      action* ac = rit->second;
      std::cout << sprockit::printf("      Rank %s: pending %s partner %d round %d join counter %d",
                    rank_str().c_str(), action::tostr(ac->type), ac->partner, ac->round, ac->join_counter)
                << std::endl;
    }
  }}
}

void
dag_collective_actor::start_recv_nack_instead(action* ac)
{
  protocol_t pr = protocol_for_action(ac);
  switch(pr){
    case eager_protocol:
    case get_protocol:
      break; //do nothing
    case put_protocol:
      send_failure_message(ac, collective_work_message::nack_put_header);
      break;
  }
}

void
dag_collective_actor::send_failure_message(
  action* ac, collective_work_message::action_t ty)
{
  //don't actually need to send this - he's already dead!
  if (is_failed(ac->partner))
    return;

  void* no_buffer = 0;
  int no_elems = 0;
  int no_size = 0;
  //send a failure message to the neighbor letting him know to abandon the collective
  collective_work_message::ptr msg = new collective_eager_message(
      type_, ty,
      no_buffer, no_elems, no_size, tag_,
      ac->round, dense_me_,
      ac->partner);
  msg->append_failed(failed_ranks_);

  send_header(ac->partner, msg);
}

void
dag_collective_actor::reput_pending(uint32_t id, pending_msg_map& pending)
{
  std::list<collective_work_message::ptr> tmp;

  {pending_msg_map::iterator it = pending.find(id);
  while (it != pending.end()){
    collective_work_message::ptr msg = it->second;
    tmp.push_back(msg);
    pending.erase(it);
    it = pending.find(id);
  }}

  {std::list<collective_work_message::ptr>::iterator it, end = tmp.end();
  for (it=tmp.begin(); it != end; ++it){
    collective_work_message::ptr msg = *it;
    incoming_message(msg);
  }}
}

void
dag_collective_actor::erase_pending(uint32_t id, pending_msg_map& pending)
{
  pending_msg_map::iterator it = pending.find(id);
  while (it != pending.end()){
    pending.erase(it);
    it = pending.find(id);
  }
}


void
dag_collective_actor::action_done(action::type_t ty, int round, int partner)
{
  uint32_t id = action::message_id(ty, round, partner);

  active_map::iterator it = active_comms_.find(id);
  if (it == active_comms_.end()){
    for (it=active_comms_.begin(); it != active_comms_.end(); ++it){
      std::cerr << "have action id " << it->first
        << " to partner " << it->second->partner << std::endl;
    }
    spkt_throw_printf(sprockit::value_error,
     "invalid action %s for round %d, partner %d",
     action::tostr(ty), round, partner);
  }
  action_done(it->second);
}

void
dag_collective_actor::data_sent(const collective_work_message::ptr& msg)
{
  action_done(action::send, msg->round(), msg->dense_recver());
}

void
dag_collective_actor::incoming_nack(action::type_t ty, const collective_work_message::ptr& msg)
{
  const std::set<int>& failed = msg->failed_procs();
  failed_ranks_.insert_all(failed);
  //got from sender, my action is recv
  action_done(ty, msg->round(), msg->dense_sender());
}

void
dag_collective_actor::data_recved(action *ac, const collective_work_message::ptr &msg, void *recvd_buffer)
{
  //we are allowed to have a null buffer
  //this just walks through the communication pattern
  //without actually passing around large payloads or doing memcpy's
  //if we end up here, we have a real buffer
  if (recv_buffer_){
    int my_domain_rank = dom_->my_domain_rank();
    int sender_domain_rank = domain_rank(msg->dense_sender());
    if (my_domain_rank == sender_domain_rank){
      do_sumi_debug_print("ignoring", rank_str().c_str(), msg->dense_sender(),
        ac->round, 0, nelems_, type_size_, recv_buffer_);
    } else {
      bool need_recv_action = ac->recv_type == action::out_of_place || msg->payload_type() == message::eager_payload;
      void* buffer_to_use = ac->recv_type == action::temp ? recv_buffer_ : result_buffer_;
      void* dst_buffer = message_buffer(buffer_to_use, ac->offset);

      //if (ac->recv_type == action::temp){
      //  printf("Rank %s receiving into temp receive buffer %p : %d\n", rank_str().c_str(), buffer_to_use);
      //} else {
      //  printf("Rank %s receiving into result buffer %p : %d\n", rank_str().c_str(), buffer_to_use);
      //}

      do_sumi_debug_print("currently",
       rank_str().c_str(), ac->partner,
       ac->round, ac->offset, ac->nelems, type_size_,
       dst_buffer);

      do_sumi_debug_print("receiving",
        rank_str().c_str(), ac->partner,
        ac->round,
        ac->offset, ac->nelems, type_size_,
        recvd_buffer);

      if (need_recv_action){
        buffer_action(dst_buffer, recvd_buffer, ac);
      }

      do_sumi_debug_print("now", rank_str().c_str(),
        ac->partner, ac->round,
        0, ac->offset + ac->nelems, type_size_,
        buffer_to_use);
    }
  }

  action_done(ac);
}

void
dag_collective_actor::data_recved(
  const collective_work_message::ptr& msg,
  void* recvd_buffer)
{
   debug_printf(sumi_collective | sumi_collective_round | sumi_collective_sendrecv,
        "Rank %s collective %s(%p) finished recving for round=%d tag=%d buffer=%p msg=%p",
        rank_str().c_str(), to_string().c_str(),
        this, msg->round(), tag_,
        (void*) recv_buffer_, msg.get());

  uint32_t id = action::message_id(action::recv, msg->round(), msg->dense_sender());
  action* ac = active_comms_[id];
  if (ac == 0){
    spkt_throw_printf(sprockit::value_error,
      "on %d, received data for unknown receive %u from %d on round %d",
      dense_me_, id, msg->dense_sender(), msg->round());
  }
  data_recved(ac, msg, recvd_buffer);
}

public_buffer
dag_collective_actor::recv_buffer(action* ac)
{
  public_buffer recv_buf = ac->recv_type != action::in_place_result ? recv_buffer_ : result_buffer_;
  recv_buf.offset_ptr(ac->offset*type_size_);
  if (result_buffer_.ptr && recv_buf.ptr == 0){
    spkt_throw(sprockit::value_error,
      "working with real payload, but somehow getting a null buffer");
  }
  return recv_buf;
}

public_buffer
dag_collective_actor::send_buffer(int offset)
{
  public_buffer send_buf = send_buffer_;
  send_buf.offset_ptr(offset*type_size_);
  if (result_buffer_.ptr && send_buf.ptr == 0){
    spkt_throw(sprockit::value_error,
      "working with real payload, but somehow sending a null buffer");
  }
  return send_buf;
}

void
dag_collective_actor::next_round_ready_to_put(
  action* ac,
  const collective_work_message::ptr& header)
{
  debug_printf(sumi_collective | sumi_collective_sendrecv | sumi_collective_round,
    "Rank %s, collective %s ready to put for round=%d tag=%d from rank %d(%d)",
    rank_str().c_str(), to_string().c_str(),
    header.get(), header->round(), tag_,
    header->dense_sender(), header->sender());

  if (failed()){
    send_failure_message(ac, collective_work_message::nack_put_payload);
    action_done(ac);
  } else {
    //reuse the header and send it back
    collective_rdma_message::ptr put_payload = ptr_safe_cast(collective_rdma_message, header);
    put_payload->set_action(collective_work_message::put_data);
    put_payload->reverse();
    put_payload->local_buffer() = send_buffer(ac->offset);

    debug_printf(sumi_collective | sumi_collective_sendrecv,
      "Rank %s, collective %s(%p) starting put %d elems at offset %d to %d(%d) for round=%d tag=%d msg %p",
      rank_str().c_str(), to_string().c_str(), this,
      ac->nelems, ac->offset,
      header->dense_sender(), header->sender(),
      ac->round, tag_, put_payload.get());

    rdma_put(put_payload->dense_recver(), put_payload);
  }
}

void
dag_collective_actor::next_round_ready_to_get(
  action* ac,
  const collective_work_message::ptr& header)
{
  debug_printf(sumi_collective | sumi_collective_sendrecv | sumi_collective_round,
    "Rank %s, collective %s received get header %p for round=%d tag=%d from rank %d",
    rank_str().c_str(), to_string().c_str(),
    header.get(), header->round(), tag_, header->sender());

  if (failed()){
    send_failure_message(ac, collective_work_message::nack_get_ack);
    action_done(ac);
  }
  else {
    //reuse the header and send it back
    collective_rdma_message::ptr get_req = ptr_safe_cast(collective_rdma_message, header);
    get_req->set_action(collective_work_message::get_data);

    get_req->local_buffer() = recv_buffer(ac);

    debug_printf(sumi_collective | sumi_collective_sendrecv,
        "Rank %s, collective %s(%p) starting get %d elems at offset %d from %d(%d) for round=%d tag=%d msg %p",
        rank_str().c_str(), to_string().c_str(), this,
        ac->nelems, ac->offset,
        header->dense_sender(), header->sender(),
        header->round(), tag_, get_req.get());

    do_sumi_debug_print("rdma get into",
       rank_str().c_str(), ac->partner,
       ac->round, ac->offset, ac->nelems, type_size_,
       get_req->local_buffer());

    do_sumi_debug_print("rdma get from",
       rank_str().c_str(), ac->partner,
       ac->round, ac->offset, ac->nelems, type_size_,
       get_req->remote_buffer());


    rdma_get(header->dense_sender(), get_req);
  }

}

void
dag_collective_actor::incoming_recv_message(action* ac, const collective_work_message::ptr& msg)
{
  switch(msg->action())
  {
  case collective_work_message::rdma_get_header:
    next_round_ready_to_get(ac, msg);
    break;
  case collective_work_message::eager_payload:
    //data recved will clear the actions
    data_recved(ac, msg, msg->eager_buffer());
    break;
  case collective_work_message::nack_get_header:
  case collective_work_message::nack_eager:
    failed_ranks_.insert_all(msg->failed_procs());
    action_done(ac);
    break;
  default:
    spkt_throw_printf(sprockit::value_error,
     "invalid recv action %s", collective_work_message::tostr(msg->action()));
  }
}

void
dag_collective_actor::incoming_send_message(action* ac, const collective_work_message::ptr& msg)
{
  switch(msg->action())
  {
  case collective_work_message::rdma_put_header:
    next_round_ready_to_put(ac, msg);
    break;
  case collective_work_message::nack_put_header:
    failed_ranks_.insert_all(msg->failed_procs());
    action_done(ac);
    break;
  default:
    spkt_throw_printf(sprockit::value_error,
     "invalid send action %s", collective_work_message::tostr(msg->action()));
  }
}

void
dag_collective_actor::incoming_message(const collective_work_message::ptr& msg)
{
  debug_printf(sumi_collective | sumi_collective_sendrecv,
    "Rank %s on incoming message with action %s from %d on round=%d tag=%d ",
    rank_str().c_str(),
    collective_work_message::tostr(msg->action()),
    msg->sender(), msg->round(), tag_);

  switch(msg->action())
  {
  case collective_work_message::nack_get_ack:
    //I told someone to do an RDMA get, but they detected a failure
    //I needed this send ack
    incoming_nack(action::send, msg);
    break;
  case collective_work_message::rdma_get_header:
  case collective_work_message::eager_payload:
  case collective_work_message::nack_get_header:
  case collective_work_message::nack_eager:
  {
    uint32_t mid = action::message_id(action::recv, msg->round(), msg->dense_sender());
    active_map::iterator it = active_comms_.find(mid);
    if (it == active_comms_.end()){
      debug_printf(sumi_collective,
         "Rank %s not yet ready for recv message from %s on round %d",
         rank_str().c_str(), rank_str(msg->dense_sender()).c_str(), msg->round());
      pending_recv_headers_.insert(std::make_pair(mid, msg));
    } else {
      action* ac = it->second;
      incoming_recv_message(ac, msg);
    }
    break;
  }
  case collective_work_message::rdma_put_header:
  case collective_work_message::nack_put_header:
  {
    debug_printf(sumi_collective,
       "Rank %s not yet ready for send message from %s on round %d",
       rank_str().c_str(), rank_str(msg->dense_recver()).c_str(), msg->round());
    uint32_t mid = action::message_id(action::send, msg->round(), msg->dense_recver());
    active_map::iterator it = active_comms_.find(mid);
    if (it == active_comms_.end()){
      pending_send_headers_.insert(std::make_pair(mid, msg));
    } else {
      action* ac = it->second;
      incoming_send_message(ac, msg);
    }
    break;
  }
  default:
    spkt_throw_printf(sprockit::value_error,
        "dag_collective_actor::incoming_message: invalid action %s",
        collective_work_message::tostr(msg->action()));
  }
}

collective_done_message::ptr
dag_collective_actor::done_msg() const
{
  collective_done_message::ptr msg = new collective_done_message(tag_, type_, dom_);
  msg->set_domain_rank(dom_->my_domain_rank());
  msg->set_result(result_buffer_.ptr);
  thread_safe_set<int>::const_iterator it, end = failed_ranks_.start_iteration();
  for (it = failed_ranks_.begin(); it != end; ++it){
    msg->append_failed(global_rank(*it));
  }
  failed_ranks_.end_iteration();
  return msg;
}

void
dag_collective_actor::put_done_notification()
{
  if (complete_){
    return; //no longer treat as error
    //beause of self messages, you could end up calling this twice
  }
  complete_ = true;

  debug_printf(sumi_collective,
    "Rank %s putting done notification on tag=%d ",
    rank_str().c_str(), tag_);

  finalize_buffers();

  my_api_->notify_collective_done(done_msg());
  timeout_ = 0;
}

void
dag_collective_actor::recv(const collective_work_message::ptr& msg)
{
  if (is_failed(msg->dense_sender())){
    //this is a lingering message that got caught in transit
    //while in transit, the parent died and we found out about it
    //drop this message and ignore it
    return;
  }

  message::payload_type_t ty = msg->payload_type();
  switch (ty)
  {
    case message::rdma_get:
      //the recv buffer was the "local" buffer in the RDMA get
      //I got it from someone locally
      data_recved(msg, msg->local_buffer());
      break;
    case message::rdma_put:
      //the recv buffer the "remote" buffer in the RDMA put
      //some put into me remotely
      data_recved(msg, msg->remote_buffer());
      break;
    case message::rdma_get_ack:
    case message::rdma_put_ack:
      data_sent(msg);
      break;
    case message::rdma_get_nack:
      //partner is the sender - I tried and RDMA get but they were dead
      incoming_nack(action::recv, msg);
      break;
    case message::header:
    case message::eager_payload:
      incoming_message(msg);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "virtual_dag_collective_actor::recv: invalid message type %s",
        message::tostr(ty));
  }
}

void
dag_collective_actor::compute_tree(int &log2nproc, int &midpoint, int &nproc) const
{
  nproc = 1;
  log2nproc = 0;
  while (nproc < dense_nproc_)
  {
    ++log2nproc;
    nproc *= 2;
  }
  midpoint = nproc / 2;
}

void
bruck_actor::compute_tree(int &log2nproc, int &midpoint, int &num_rounds, int &nprocs_extra_round) const
{
  int virtual_nproc;
  dag_collective_actor::compute_tree(log2nproc, midpoint, virtual_nproc);
  nprocs_extra_round = 0;
  num_rounds = log2nproc;
  if (dense_nproc_ != virtual_nproc){
    --num_rounds;
    //we will have to do an extra exchange in the last round
    nprocs_extra_round = dense_nproc_ - midpoint;
  }
}

int
virtual_rank_map::real_to_virtual(int rank, int* ret) const
{
  int num_actors_two_roles = virtual_nproc_ - nproc_;
  if (rank >= num_actors_two_roles){
    ret[0] = num_actors_two_roles + rank;
    return 1;
  }
  else {
    ret[0] = 2*rank;
    ret[1] = ret[0] + 1;
    return 2;
  }
}

int
virtual_rank_map::virtual_to_real(int virtual_rank) const
{
  //these are the guys who have to do two roles because
  //we don't have an exact power of two
  int num_actors_two_roles = virtual_nproc_ - nproc_;
  if (virtual_rank >= 2*num_actors_two_roles){
    int real_rank = virtual_rank - num_actors_two_roles;
    return real_rank;
  }
  else {
    int real_rank = virtual_rank / 2;
    return real_rank;
  }
}


void*
dag_collective_actor::message_buffer(void* buffer, int offset)
{
  if (buffer){
    int total_stride = type_size_ * offset;
    char* tmp = ((char*)buffer) + total_stride;
    return tmp;
  }
  //nope, no buffer
  return 0;
}

std::string
collective_actor::failed_proc_string() const
{
  std::stringstream sstr;
  sstr << "{";
  thread_safe_set<int>::const_iterator it, end = failed_ranks_.start_iteration();
  for (it = failed_ranks_.begin(); it != end; ++it){
    sstr << " " << *it;
  }
  failed_ranks_.end_iteration();
  sstr << " }";
  return sstr.str();
}

}
