/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */


#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request.h>
#include <sumi-mpi/mpi_queue/mpi_queue_probe_request.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>
#include <sprockit/statics.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app_id.h>

#include <stdint.h>

RegisterNamespaces("traffic_matrix", "num_messages");
RegisterKeywords(
"mpi_delay",
"mpi_implementation",
"envelope",
"smp_single_copy_size",
"max_eager_msg_size",
"max_vshort_msg_size",
"mpi_spyplot",
"mpi_queue_post_rdma_delay",
"mpi_queue_post_header_delay",
"mpi_queue_poll_delay",
"post_rdma_delay",
"post_header_delay",
"poll_delay",
"implementation",
);

DeclareDebugSlot(mpi_all_sends);
RegisterDebugSlot(mpi_all_sends);


namespace sumi {

static sprockit::need_delete_statics<mpi_queue> del_statics;

bool
mpi_queue::sortbyseqnum::operator()(const mpi_message::ptr& a,
                                    const mpi_message::ptr&b) const
{
  return (a->seqnum() < b->seqnum());
}

mpi_queue::mpi_queue(sprockit::sim_parameters* params,
                     sstmac::sw::software_id sid,
                     mpi_api* api) :
  next_id_(0),
  taskid_(sid.task_),
  appid_(sid.app_),
  api_(api),
  user_lib_mem_(nullptr),
  user_lib_time_(nullptr),
  os_(api->os()),
  spy_num_messages_(nullptr),
  spy_bytes_(nullptr)
{
  max_vshort_msg_size_ = params->get_optional_byte_length_param("max_vshort_msg_size", 512);
  max_eager_msg_size_ = params->get_optional_byte_length_param("max_eager_msg_size", 8192);

  post_rdma_delay_ = params->get_optional_time_param("post_rdma_delay", 0);
  post_header_delay_ = params->get_optional_time_param("post_header_delay", 0);
  poll_delay_ = params->get_optional_time_param("poll_delay", 0);

  spy_num_messages_ = sstmac::optional_stats<sstmac::stat_spyplot>(os_->node(),
        params, "traffic_matrix", "ascii", "num_messages");
  spy_bytes_ = sstmac::optional_stats<sstmac::stat_spyplot>(os_->node(),
        params, "traffic_matrix", "ascii", "bytes");

  user_lib_mem_ = new sstmac::sw::lib_compute_memmove(params, "mpi_queue-user-lib-mem", sid, os_);

  user_lib_time_ = new sstmac::sw::lib_compute_time(params, "mpi_queue-user-lib-time", sid, os_);

  mpi_queue_debug("init on node %d", int(operating_system::current_node_id()));

  next_id_ = uint64_t(taskid_) << 32;
}

void
mpi_queue::delete_statics()
{
}

double
mpi_queue::now() const {
  return os_->now().sec();
}

mpi_queue::~mpi_queue() throw ()
{
  //receives can be posted, but not resolved
  //clean up stuff
  for (mpi_queue_recv_request* req : pending_message_){
    delete req;
  }
  for (mpi_queue_recv_request* req : waiting_message_){
    delete req;
  }
  for (auto& pair : recv_needs_payload_){
    delete pair.second;
  }

  delete user_lib_mem_;
  delete user_lib_time_;
}

mpi_message::ptr
mpi_queue::send_message(int count, MPI_Datatype type,
                int dst_rank, int tag, mpi_comm* comm)
{
  mpi_type* typeobj = api_->type_from_id(type);
  if (typeobj->packed_size() < 0){
    spkt_throw_printf(sprockit::value_error,
      "MPI_Datatype %s has negative size %ld",
      api_->type_str(type).c_str(), typeobj->packed_size());
  }
  long bytes = count * int64_t(typeobj->packed_size());
  mpi_protocol* prot = protocol(bytes);
  mpi_queue_debug("starting send count=%d, type=%s, dest=%d, tag=%d, comm=%s, prot=%s",
    count, api_->type_str(type).c_str(), int(dst_rank),
    int(tag), api_->comm_str(comm).c_str(),
    prot->to_string().c_str());
  task_id dst_tid = comm->peer_task(dst_rank);
  mpi_message::ptr mess = new mpi_message(comm->rank(), dst_rank,
                          count, type, typeobj->packed_size(),
                          tag, comm->id(),
                          next_outbound_[dst_tid]++,
                          next_id_++, prot);

  if (spy_num_messages_) spy_num_messages_->add_one(int(taskid_), dst_tid);
  if (spy_bytes_) spy_bytes_->add(int(taskid_), dst_tid, bytes);
  return mess;
}


void
mpi_queue::configure_send_request(const mpi_message::ptr& mess,
                                  mpi_request* key)
{
  mpi_queue_send_request* req = new mpi_queue_send_request(mess, key, this);

  /** push this on first. important! */
  mess->set_needs_send_ack(false);
  if (mess->protocol()->send_needs_nic_ack()) {
    mess->set_needs_send_ack(true);
    send_needs_nic_ack_.push_back(req);
  }
  else if (mess->protocol()->send_needs_eager_ack()) {
    send_needs_eager_ack_.push_back(req);
  }
  else if (mess->protocol()->send_needs_completion_ack()) {
    send_needs_completion_ack_[mess->unique_int()] = req;
  }
}

void
mpi_queue::send(mpi_request *key, int count, MPI_Datatype type,
  int dest, int tag, mpi_comm *comm, void *buffer)
{
  mpi_message::ptr mess = send_message(count, type, dest, tag, comm);
  configure_send_request(mess, key);

  if (dest >= comm->size()){
    spkt_throw_printf(sprockit::value_error,
               "mpi_queue::send: sending to destination %d on MPI_Comm %d,"
               " but maximum rank is %d", dest, comm->id(), comm->size());
  }

#if !SSTMAC_ALLOW_LARGE_PAYLOADS
  if (buffer && mess->byte_length() > 64){
    spkt_abort_printf("mpi queue sending large message with real payload:\n%s",
      mess->to_string().c_str());
  }
#endif

  //either return the original buffer or create a new one for eager
  if (buffer){
    mess->protocol()->configure_send_buffer(mess, buffer);
  }

  mess->protocol()->send_header(this, mess);
}

mpi_protocol*
mpi_queue::protocol(long bytes) const
{
  if (bytes <= max_vshort_msg_size_) {
    return mpi_protocol::eager0_protocol;
  }
  else if (bytes <= max_eager_msg_size_) {
    return mpi_protocol::eager1_singlecpy_protocol;
  }
  else {
    return mpi_protocol::rendezvous_protocol;
  }
}

mpi_message::ptr
mpi_queue::find_matching_recv(mpi_queue_recv_request* req)
{
  need_recv_t::iterator it, end = need_recv_.end();
  for (it = need_recv_.begin(); it != end; ++it) {
    mpi_message::ptr mess = *it;
    if (req->matches(mess)) {
      mpi_queue_debug("matched recv tag=%s,src=%s to send tag=%d,src=%d on comm=%s",
        api_->tag_str(req->tag_).c_str(), api_->src_str(req->source_).c_str(),
        mess->tag(), mess->src_rank(), api_->comm_str(req->comm_).c_str());

      need_recv_.erase(it);
      return mess;

    }
  }
  mpi_queue_debug("could not match recv tag=%s, src=%s to any sends on comm=%s",
    api_->tag_str(req->tag_).c_str(), api_->src_str(req->source_).c_str(),
    api_->comm_str(req->comm_).c_str());

  pending_message_.push_back(req);
  return mpi_message::ptr();
}

//
// Receive data.
//
void
mpi_queue::recv(mpi_request* key, int count,
                MPI_Datatype type,
                int source, int tag,
                mpi_comm* comm,
                void* buffer)
{
  mpi_queue_debug("starting recv count=%d, type=%s, src=%s, tag=%s, comm=%s",
        count, api_->type_str(type).c_str(), api_->src_str(source).c_str(),
        api_->tag_str(tag).c_str(), api_->comm_str(comm).c_str());

#if !SSTMAC_ALLOW_LARGE_PAYLOADS
  if (buffer && count > 16){
    spkt_abort_printf("mpi queue recving large message with real payload");
  }
#endif

  mpi_queue_recv_request* req = new mpi_queue_recv_request(key, this,
                            count, type, source, tag, comm->id(), buffer);

  mpi_message::ptr mess = find_matching_recv(req);
  if (mess) {
    if (mess->in_flight()){
      //this is awkward - I match this pending message
      //but I can't do anything to complete it
      //the message has already been processed (hence the seq ignore)
      in_flight_messages_.push_back(req);
    }
    else if (mess->is_payload()) {
      //user_lib_mem_->copy(mess->payload_bytes());
      mess->protocol()->incoming_payload(this, mess, req);
    }
    else {
      mess->protocol()->incoming_header(this, mess, req);
    }
  }
}

void
mpi_queue::finalize_recv(const mpi_message::ptr& msg,
                         mpi_queue_recv_request* req)
{
  req->key_->complete(msg);
#if SSTMAC_COMM_SYNC_STATS
  req->key_->set_time_sent(msg->time_sent());
  req->key_->set_time_arrived(msg->time_arrived());
#endif
  delete req;
}

//
// Ask for a notification when a message with the given signature arrives.
//
void
mpi_queue::probe(mpi_request* key, mpi_comm* comm,
                 int source, int tag)
{
  mpi_queue_debug("starting probe src=%s, tag=%s, comm=%s",
    api_->src_str(source).c_str(), api_->tag_str(tag).c_str(),
    api_->comm_str(comm).c_str());

  mpi_queue_probe_request* req = new mpi_queue_probe_request(key, comm->id(), source, tag);
  // Figure out whether we already have a matching message.
  need_recv_t::iterator it, end = need_recv_.end();
  for (it = need_recv_.begin(); it != end; ++it) {
    mpi_message::ptr mess = *it;
    if (req->matches(mess)){
      // We're good to go.
      req->complete(mess);
      return;
    }
  }
  // If we get here, we still need to wait for the message.
  probelist_.push_back(req);
}

//
// Immediate-mode probe for a message with the given signature.
//
bool
mpi_queue::iprobe(mpi_comm* comm,
                  int source,
                  int tag,
                  MPI_Status* stat)
{
  mpi_queue_debug("starting immediate probe src=%s, tag=%s, comm=%s",
    api_->src_str(source).c_str(), api_->tag_str(tag).c_str(),
    api_->comm_str(comm).c_str());

  mpi_queue_probe_request* req = new mpi_queue_probe_request(NULL, comm->id(), source, tag);
  need_recv_t::iterator it, end = need_recv_.end();
  for (it = need_recv_.begin(); it != end; ++it) {
    mpi_message::ptr mess = *it;
    if (req->matches(mess)) {
      // This is it
      delete req;
      if (stat != MPI_STATUS_IGNORE) mess->build_status(stat);
      return true;
    }
  }
  // If we got here, there was no match
  delete req;
  return false;
}

void
mpi_queue::send_completion_ack(const mpi_message::ptr& message)
{
  mpi_queue_debug("send completion ack for %s", message->to_string().c_str());

  //need to send an ack back to sender
  int dst = message->sender();
  message->payload_to_completion_ack();
  api_->send_header(dst, message);
}

void
mpi_queue::incoming_progress_loop_message(const mpi_message::ptr& message)
{
  mpi_queue_debug("have incoming %p message %s", 
    message.get(), message->to_string().c_str());

  if (message->is_nic_ack()) {
    handle_nic_ack(message);
    return;
  }

  /// These are the types of messages we may expect:
  /// (1)  Response to handshake request (we have the requestor).
  /// (2)  Data delivery following a handshake ack (we have the receiver)
  /// (3)  Acks from the NIC that a message has left
  /// (4)  New messages (either eager-send or a handshake request).
  /// Only category (4) is subject to order requirements.  The other
  /// categories get handled immediately

  switch (message->content_type()) {
    case mpi_message::eager_payload:
    case mpi_message::header:
      this->incoming_new_message(message);
      break;
    case mpi_message::completion_ack:
      this->incoming_completion_ack(message);
      break;
    case mpi_message::data:
      message->protocol()->incoming_payload(this, message);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
         "received invalid messgae content type %s in incoming message",
         mpi_message::str(message->content_type()));
  }
}

//
// Handle a response to an ack posted from this queue.
//
void
mpi_queue::incoming_completion_ack(const mpi_message::ptr& message)
{

  mpi_message::id id = message->unique_int();
  mpi_queue_debug("queue has incoming send completion ack from %d for unique id %llu",
     message->sender(), int(message->unique_int()));

  ack_needed_t::iterator it =
    send_needs_completion_ack_.find(message->unique_int());
  if (it == send_needs_completion_ack_.end()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpi_queue[%d]::incoming_message: completion ack with no match on %s",
                     taskid_, message->to_string().c_str());
  }

  mpi_queue_send_request* req = it->second;
  send_needs_completion_ack_.erase(it);
  req->complete(message);
  delete req;
}

//
// Handle a new incoming message.  Can be either an eager send (with data)
// or a new handshake request
//
void
mpi_queue::incoming_new_message(const mpi_message::ptr& message)
{
  mpi_queue_debug("incoming new message %s", message->to_string().c_str());

  task_id tid(message->sender())  ;

  if (message->seqnum() == next_inbound_[tid]) {
    mpi_queue_debug("seqnum for task %d matched expected seqnum %d and advanced to next seqnum",
        int(tid), int(next_inbound_[tid]));

    handle_new_message(message);
    ++next_inbound_[tid];

    // Handle any messages that have been freed by the arrival of this one
    if (!held_[tid].empty()) {
      hold_list_t::iterator it = held_[tid].begin(), end = held_[tid].end();
      while (it != end) {
        mpi_message::ptr mess = *it;
        mpi_queue_debug("handling out-of-order message for task %d, seqnum %d",
            int(tid), mess->seqnum());
        if (mess->seqnum() <= next_inbound_[tid]) {
          //it = held_[tid].erase(it);
          it++;
          handle_new_message(mess);
          ++next_inbound_[tid];
        }
        else {
          break;
        }
      }
      // Now erase all the completed messages from the held queue.
      held_[tid].erase(held_[tid].begin(), it);
    }
  }
  else if (message->in_flight()) {
    mpi_queue_debug("got RDMA message and ignoring seqnum");
    handle_new_message(message);
  }
  else {
    if (message->seqnum() < next_inbound_[tid]){
      //how did we go backwards!?!??!
      spkt_throw_printf(sprockit::value_error,
        "mpi_queue::incoming message: sequence number has gone backwards from %d to %d "
        "for rank %d receving from %d for message %s",
        next_inbound_[tid], message->seqnum(),
        int(taskid_), int(tid),
        message->to_string().c_str());
    }

    mpi_queue_debug("message arrived out-of-order with seqnum %d, didn't match expected %d for task %d",
        message->seqnum(), int(next_inbound_[tid]), int(tid));

    held_[tid].insert(message);
  }
}

void
mpi_queue::notify_probes(const mpi_message::ptr& message)
{
  probelist_t::iterator tmp,
    pit = probelist_.begin(),
    pend = probelist_.end();
  while (pit != pend) {
    tmp = pit++;
    mpi_queue_probe_request* preq = *tmp;
    if (preq->matches(message)){
      probelist_.erase(tmp);
      preq->complete(message);
      delete preq;
    }
  }
}

void
mpi_queue::handle_new_message(const mpi_message::ptr& message)
{
  switch (message->content_type())
  {
    case mpi_message::eager_payload:
      message->protocol()->incoming_payload(this, message);
      break;
    case mpi_message::header:
      message->protocol()->incoming_header(this, message);
      break;
    default:
      spkt_throw(sprockit::value_error,
        "mpi_queue::handle_new_message: invalid new message type %s",
        mpi_message::str(message->content_type()));
  }
}

//
// Complete an inbound message.
//
mpi_queue_recv_request*
mpi_queue::pop_matching_request(pending_message_t &pending,
                        const mpi_message::ptr& message)
{
  mpi_queue_recv_request* req;
  pending_message_t::iterator it, end, tmp;
  end = pending.end();
  for (it = pending.begin(); it != end;) {
    req = *it;
    tmp = it++;
    if (req->is_cancelled()) {
      pending.erase(tmp);
    }
    else if (req->matches(message)) {
      pending.erase(tmp);
      return req;
    }
  }
  return 0;
}

mpi_queue_recv_request*
mpi_queue::pop_pending_request(const mpi_message::ptr& message,
                                bool set_need_recv)
{
  mpi_queue_recv_request* req = pop_matching_request(pending_message_, message);
  if (!req) {
    // We get here if no match was found.
    // Messages that don't have a respondent are added to the list
    mpi_queue_debug("receiver not ready for message %s: %d recvs posted",
        message->to_string().c_str(), pending_message_.size());

    if (set_need_recv) {
      need_recv_.push_back(message);
    }
  }
  return req;
}

mpi_queue_recv_request*
mpi_queue::pop_waiting_request(const mpi_message::ptr& message)
{
  mpi_queue_recv_request* req = pop_matching_request(waiting_message_, message);
  if (!req) {
    std::cerr << message->to_string() << std::endl;
    spkt_throw_printf(sprockit::value_error,
             "MPI rank %d: matching request not found for message",
             int(api()->comm_world()->rank()));
  }
  return req;
}



void
mpi_queue::complete_nic_ack(const mpi_message::ptr& message)
{
  mpi_queue_debug("handle nic ack for message %s",
                  message->to_string().c_str());

  send_needs_ack_t::iterator it, end = send_needs_nic_ack_.end();
  for (it = send_needs_nic_ack_.begin(); it != end; ++it) {
    mpi_queue_send_request* sreq = *it;
    if (sreq->matches(message)) {
      mpi_queue_debug("complete nic ack match found");
      send_needs_nic_ack_.erase(it);
      sreq->complete(message);
      delete sreq;
      return;
    }
  }
  spkt_throw_printf(sprockit::value_error,
    "no match found for nic ack on %s",
    message->to_string().c_str());
}

void
mpi_queue::handle_nic_ack(const mpi_message::ptr& message)
{
  mpi_protocol* prot = message->protocol();
  prot->handle_nic_ack(this, message);
}

}

