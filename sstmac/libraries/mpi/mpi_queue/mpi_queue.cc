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


#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_send_request.h>
#include <sstmac/libraries/mpi/rma/mpi_queue_get_request.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_probe_request.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/rma/mpi_rma_message.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/messages/payload.h>
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

RegisterNamespaces("traffic_matrix");
DeclareDebugSlot(mpi_all_sends);
RegisterDebugSlot(mpi_all_sends);

ImplementFactory(sstmac::sw::mpi_queue);

namespace sstmac {
namespace sw {

static sprockit::need_delete_statics<mpi_queue> del_statics;

bool
mpi_queue::sortbyseqnum::operator()(const mpi_message::ptr& a,
                                    const mpi_message::ptr&b) const
{
  return (a->seqnum() < b->seqnum());
}

void
mpi_queue::set_event_parent(event_scheduler* m)
{
  server_->set_event_parent(m);
#if !SSTMAC_INTEGRATED_SST_CORE
  if (spy_num_messages_) m->register_stat(spy_num_messages_);
  if (spy_bytes_) m->register_stat(spy_bytes_);
#endif
}

//
// Hi there.
//
mpi_queue::mpi_queue() :
  mintrans_(0),
  next_id_(0),
  taskid_(0),
  appid_(0),
  mpi_impl_(0),
  user_lib_mem_(0),
  user_lib_time_(0),
  os_(0),
  server_(0),
  spy_num_messages_(0),
  spy_bytes_(0)
{
}

void
mpi_queue::finalize_init()
{
  // need to debug dangling messages.
  next_id_ = mpi_message::id(int(taskid_) * int64_t(4000000000LL));
}

void
mpi_queue::set_mpi_server(mpi_server* serv)
{
  server_ = serv;
  mintrans_ = 0;
  os_ = serv->os();

  std::string libname = sprockit::printf("mpi_queue-user-lib-mem-%d-%d",
                                  int(taskid_), int(appid_));
  user_lib_mem_ = new lib_compute_memmove(libname);
  os_->register_lib(this, user_lib_mem_);

  user_lib_time_ = lib_compute_time::construct(
                     sprockit::printf("mpi_queue-user-lib-time-%d-%d", int(taskid_), int(appid_)));
  os_->register_lib(this, user_lib_time_);

  mpi_queue_debug("init on node %d",
    int(operating_system::current_node_id()));
}

void
mpi_queue::init_param1(const software_id& myid)
{
  taskid_ = myid.task_;
  appid_ = myid.app_;
}

void
mpi_queue::init_factory_params(sprockit::sim_parameters* params)
{
  /**
    sstkeyword {
        docstring=The underlying protocols/hardware features that MPI supports.;
    }
  */
  mpi_impl_ = mpi_implementation_factory::get_optional_param("implementation", "basic", params);

  if (params->has_namespace("traffic_matrix")){
    sprockit::sim_parameters* tparams = params->get_namespace("traffic_matrix");
    spy_bytes_ = test_cast(stat_spyplot, stat_collector_factory::get_optional_param("type", "spyplot_png", tparams));
    spy_num_messages_ = test_cast(stat_spyplot, stat_collector_factory::get_optional_param("type", "spyplot_png", tparams));
    if (!spy_bytes_){
      spkt_throw(sprockit::value_error,
        "MPI spyplot specified as %s, must be spyplot or spyplot_png",
        params->get_param("type").c_str());
    }
  }
}

void
mpi_queue::unregister_all_libs()
{
  server_->os()->unregister_all_libs(this);
}

void
mpi_queue::delete_statics()
{
}

//
// Goodbye.
//
mpi_queue::~mpi_queue() throw ()
{
  delete mpi_impl_;
}

void
mpi_queue::get(mpi_request* key, int count, mpi_type_id type,
               mpi_id dest, mpi_comm* comm,
               mpi_window* win, const mpi_rma_message::op_info& op,
               event_handler* completion)
{
  mpi_queue_debug("queue starting get count=%d, type=%s, dest=%d, comm=%s",
    count, api_->type_str(type).c_str(), int(dest), api_->comm_str(comm).c_str());

  task_id dst_tid = comm->peer_task(dest);
  task_id src_tid = comm->my_task();
  mpi_id src_rank = comm->rank();

  node_id src = comm->node_at(src_rank);
  node_id dst = comm->node_at(dest);

  bool intranode = src == dst;
  mpi_type* typeobj = api_->type_from_id(type);
  long bytes = count * int64_t(typeobj->packed_size());
  mpi_protocol* protocol = mpi_protocol::rendezvous_rma_protocol;

  bool handshake_only = protocol->handshake_only();
  int msg_count = -count; //negative size indicates handshake

  timestamp sendtime(1e-9); // in the future, can make this separate send to the ami or something

  //even if it carries the payload, we still tag this as a header since it's the first
  //message sent
  mpi_message::content_type_t ctype = mpi_message::header;
  mpi_rma_message::rmatype_t rmat = mpi_rma_message::get;

  mpi_rma_message::ptr mess = new mpi_rma_message(
                                server_->lib_name(), 
                                mpi_impl_->envelope_size(),
                                mintrans_, msg_count,
                                type, typeobj->packed_size(),
                                src_rank, dest, comm->id(),
                                next_outbound_[dst_tid]++, next_id_++, mpi_message::user, src_tid,
                                dst_tid, comm->app(), ctype, payload::const_ptr(), protocol,
                                rmat, win->winid(), op);
  mess->fromaddr_ = src;
  mess->toaddr_ = dst;

  mpi_queue_get_request* req = mpi_queue_get_request::construct(key, this, completion);

  if (server_ == 0) {
    spkt_throw(sprockit::null_error,
              "mpi_queue::get: null server pointer.");
  }

  /** push this on first. important! */
  mess->set_needs_ack(false);
  gets_waiting_for_data_[mess->unique_mpi_id()] = req;


  if (spy_num_messages_) spy_num_messages_->add_one(int(taskid_), dst_tid);
  if (spy_bytes_) spy_bytes_->add(int(taskid_), dst_tid, bytes);

  do_send(mess);

}

void
mpi_queue::put(mpi_request* key, int count, mpi_type_id type,
               mpi_id dest, mpi_comm* comm,
               mpi_window* win, const mpi_rma_message::op_info& op,
               event_handler* completion, const payload::const_ptr& content)
{

  mpi_queue_debug("queue starting get count=%d, type=%s, dest=%d, comm=%s",
    count, api_->type_str(type).c_str(), int(dest), api_->comm_str(comm).c_str());

  task_id dst_tid = comm->peer_task(dest);
  task_id src_tid = comm->my_task();
  mpi_id src_rank = comm->rank();

  node_id src = comm->node_at(src_rank);
  node_id dst = comm->node_at(dest);

  bool intranode = src == dst;
  mpi_type* typeobj = api_->type_from_id(type);
  long bytes = count * int64_t(typeobj->packed_size());
  mpi_protocol* protocol = mpi_protocol::rendezvous_rma_protocol;

  bool handshake_only = protocol->handshake_only();
  int msg_count = count; //positive size indicates data

  timestamp sendtime(1e-9); // in the future, can make this separate send to the ami or something

  //even if it carries the payload, we still tag this as a header since it's the first
  //message sent
  mpi_message::content_type_t ctype = mpi_message::header;
  mpi_rma_message::rmatype_t rmat = op.type_;

  mpi_rma_message::ptr mess = new mpi_rma_message(
                                server_->lib_name(), 
                                mpi_impl_->envelope_size(),
                                mintrans_, msg_count,
                                type, typeobj->packed_size(),
                                src_rank, dest, comm->id(),
                                next_outbound_[dst_tid]++, next_id_++, mpi_message::user, src_tid,
                                dst_tid, comm->app(), ctype, content, protocol,
                                rmat, win->winid(), op);
  mess->fromaddr_ = src;
  mess->toaddr_ = dst;

  mpi_queue_send_request* req = new mpi_queue_send_request(mess, key, this, completion);

  if (server_ == 0) {
    spkt_throw(sprockit::null_error,
       "mpi_queue::get: null server pointer.");
  }

  /** push this on first. important! */
  mess->set_needs_ack(false);
  send_needs_completion_ack_[mess->unique_mpi_id()] = req;

  do_send(mess);

  mpi_queue_debug("finished init put");
}

mpi_message::ptr
mpi_queue::send_message(int count, mpi_type_id type,
                mpi_id dst_rank, mpi_tag tag, mpi_comm* comm,
                const sendinfo &sinfo, mpi_message::category message_type)
{
  mpi_queue_debug("starting send count=%d, type=%s, dest=%d, tag=%d, comm=%s, cat=%s",
    count, api_->type_str(type).c_str(), int(dst_rank),
    int(tag), api_->comm_str(comm).c_str(), mpi_message::str(message_type));

  task_id dst_tid = comm->peer_task(dst_rank);
  task_id src_tid = comm->my_task();
  mpi_id src_rank = comm->rank();

  node_id src = comm->node_at(src_rank);
  node_id dst = comm->node_at(dst_rank);

  bool intranode = src == dst;
  mpi_type* typeobj = api_->type_from_id(type);
  long bytes = count * int64_t(typeobj->packed_size());
  mpi_protocol* prot = protocol(bytes, intranode, sinfo.ssend, sinfo.onesided);

  timestamp sendtime(1e-9); // in the future, can make this separate send to the ami or something

  //even if it carries the payload, we still tag this as a header since it's the first
  //message sent

  mpi_message::ptr mess = new mpi_message(server_->lib_name(),
                          mpi_impl_->envelope_size(), mintrans_, count,
                          type, typeobj->packed_size(),
                          src_rank, dst_rank, tag, comm->id(), next_outbound_[dst_tid]++,
                          next_id_++, sinfo.ssend, message_type, src_tid, dst_tid,
                          comm->app(), prot);
  mess->fromaddr_ = src;
  mess->toaddr_ = dst;
  //by default, everything starts as basic payload type
  mess->hw::network_message::set_type(hw::network_message::payload);

  if (spy_num_messages_) spy_num_messages_->add_one(int(taskid_), dst_tid);
  if (spy_bytes_) spy_bytes_->add(int(taskid_), dst_tid, bytes);
  return mess;
}


void
mpi_queue::configure_send_request(const mpi_message::ptr& mess, mpi_request* key, event_handler* completion)
{
  mpi_queue_send_request* req = new mpi_queue_send_request(mess, key, this, completion);

  /** push this on first. important! */
  mess->set_needs_ack(false);
  if (mess->protocol()->send_needs_nic_ack()) {
    mess->set_needs_ack(true);
    send_needs_nic_ack_.push_back(req);
  }
  else if (mess->protocol()->send_needs_eager_ack()) {
    send_needs_eager_ack_.push_back(req);
  }
  else if (mess->protocol()->send_needs_rendezvous_ack()) {
    send_needs_rendezvous_ack_[mess->unique_mpi_id()] = req;
  }
  else if (mess->protocol()->send_needs_completion_ack()) {
    send_needs_completion_ack_[mess->unique_mpi_id()] = req;
  }
}

void
mpi_queue::send_debug_printf(const mpi_message::ptr& mess)
{
  debug_printf(sprockit::dbg::mpi_all_sends,
    "%6d %16ld %6d %6d %12ld %12s",
    0/*epoch*/, os_->now().ticks_int64(),
    int(mess->source_task()),
    int(mess->dest_task()),
    mess->payload_bytes(),
    mpi_message::str(mess->cat()));
}

//
// Send data.
//
void
mpi_queue::send(mpi_request* key, int count, mpi_type_id type,
                mpi_id dst_rank, mpi_tag tag, mpi_comm* comm,
                const sendinfo &sinfo, mpi_message::category message_type,
                event_handler* completion, const payload::const_ptr& content)
{
  mpi_message::ptr mess = send_message(count, type, dst_rank, tag, comm, sinfo, message_type);
  mess->set_content(content);
  configure_send_request(mess, key, completion);

  send_debug_printf(mess);
  do_send(mess);

#if SSTMAC_ENABLE_MPI_TIMELINE
  mess->set_start_time(server_->now());
#endif
}

void
mpi_queue::send(mpi_request *key, int count, mpi_type_id type,
    mpi_id dest, mpi_tag tag, mpi_comm *comm, const sendinfo &sinfo,
    mpi_message::category message_type, event_handler *completion, void *buffer)
{
  mpi_message::ptr mess = send_message(count, type, dest, tag, comm, sinfo, message_type);
  configure_send_request(mess, key, completion);
  //either return the original buffer or create a new one for eager
  if (buffer){
    mess->protocol()->configure_send_buffer(mess, buffer);
  }

  send_debug_printf(mess);

  do_send(mess);

#if SSTMAC_ENABLE_MPI_TIMELINE
  mess->set_start_time(server_->now());
#endif
}

void
mpi_queue::handle_put(const mpi_message::ptr& message)
{

}

mpi_protocol*
mpi_queue::protocol(long bytes, bool intranode, bool rsend,
                        bool onesided) const
{
  return mpi_impl_->get_protocol(bytes, intranode, rsend, onesided);
}

void
mpi_queue::start_recv(mpi_queue_recv_request* req)
{
  mpi_message::ptr mess = find_matching_recv(req);
  if (mess) {
    //if rendezvous protocol, this will never be > 0
    //if eager protocol, race condition
    if (mess->is_payload()) {
      buffered_recv(mess, req);
    }
    else {
      req->handle(mess);
    }
  }
}

void
mpi_queue::start_send(const mpi_message::ptr& msg)
{
  mpi_protocol* prot = msg->protocol();
  prot->send_header(this, msg);
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
        int(mess->tag()), int(mess->source()), api_->comm_str(req->comm_).c_str());

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
                mpi_type_id type,
                mpi_id source, mpi_tag tag,
                mpi_comm* comm,
                mpi_message::category message_type,
                event_handler* completion,
                void* buffer)
{
  mpi_queue_debug("starting recv count=%d, type=%s, src=%s, tag=%s, comm=%s, cat=%s",
        count, api_->type_str(type).c_str(), api_->src_str(source).c_str(),
        api_->tag_str(tag).c_str(), api_->comm_str(comm).c_str(),
        mpi_message::str(message_type));

  mpi_queue_recv_request* req = new mpi_queue_recv_request(key, this,
                            count, type, source, tag, comm, message_type, completion);

  req->buffer_ = buffer;
  do_recv(req);
}

void
mpi_queue::finalize_recv(const mpi_message::ptr& msg)
{
  //spy_congestion_->add(msg->source(), taskid_, msg->get_delay().msec());
}

//
// Ask for a notification when a message with the given signature arrives.
//
void
mpi_queue::probe(mpi_request* key, mpi_comm* comm,
                 mpi_id source, mpi_tag tag, event_handler* completion)
{
  mpi_queue_debug("starting probe src=%s, tag=%s, comm=%s",
    api_->src_str(source).c_str(), api_->tag_str(tag).c_str(),
    api_->comm_str(comm).c_str());

  mpi_queue_probe_request* req = new mpi_queue_probe_request(key, comm, source, tag, completion);
  // Figure out whether we already have a matching message.
  need_recv_t::iterator it, end = need_recv_.end();
  for (it = need_recv_.begin(); it != end; ++it) {
    mpi_message::ptr mess = *it;
    if (mess->cat() == mpi_message::user && req->matches(mess)){
      // We're good to go.
      req->complete(mess);
      delete req;
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
                  mpi_id source,
                  mpi_tag tag,
                  mpi_status* stat)
{
  mpi_queue_debug("starting immediate probe src=%s, tag=%s, comm=%s",
    api_->src_str(source).c_str(), api_->tag_str(tag).c_str(),
    api_->comm_str(comm).c_str());

  mpi_queue_probe_request* req = new mpi_queue_probe_request(NULL, comm, source, tag, NULL);
  need_recv_t::iterator it, end = need_recv_.end();
  for (it = need_recv_.begin(); it != end; ++it) {
    mpi_message::ptr mess = *it;
    if (mess->cat() == mpi_message::user && req->matches(mess)) {
      // This is it
      delete req;
      mess->build_status(stat);
      return true;
    }
  }
  // If we got here, there was no match
  delete req;
  return false;
}

//
// Used when a receive operation sends an ack.
//
void
mpi_queue::send_rendezvous_ack(const mpi_message::ptr& message)
{
  mpi_queue_debug("send rendezvous ack for %s",
    message->to_string().c_str());

  message->header_to_rendezvous_ack();
  server_->send(message);
}

void
mpi_queue::send_completion_ack(const mpi_message::ptr& message)
{
  mpi_queue_debug("send completion ack for %s", message->to_string().c_str());

  mpi_message::ptr copy = message->clone();
  copy->payload_to_completion_ack();
  server_->send(copy);
}

//
// CALLBACK used by mpiserver when this object has a message.
//
void
mpi_queue::incoming_message(const mpi_message::ptr& message)
{
  mpi_queue_debug("have incoming message %s", message->to_string().c_str());

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
      this->incoming_new_message(message);
      break;
    case mpi_message::rendezvous_ack:
      this->incoming_rendezvous_ack(message);
      break;
    case mpi_message::completion_ack:
      this->incoming_completion_ack(message);
      break;
    case mpi_message::data:
      message->protocol()->incoming_payload(this, message);
      break;
    case mpi_message::header:
      this->incoming_new_message(message);
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
  const mpi_message::id &id = message->unique_mpi_id();
  mpi_queue_debug("queue has incoming send completion ack from %d for unique id %llu",
     int(message->source_task()), int(message->unique_mpi_id()));

  ack_needed_t::iterator it =
    send_needs_completion_ack_.find(message->unique_mpi_id());
  if (it == send_needs_completion_ack_.end()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpi_queue[%s]::incoming_message: completion ack with no match on %s",
                     taskid_.to_string().c_str(), message->to_string().c_str());
  }

  mpi_queue_send_request* req = it->second;
  send_needs_completion_ack_.erase(it);
  req->complete(message);
  delete req;
}

//
// Handle a response to an ack posted from this queue.
//
void
mpi_queue::incoming_rendezvous_ack(const mpi_message::ptr& message)
{
  const mpi_message::id &id = message->unique_mpi_id();
  mpi_queue_debug("has incoming rendezvous ack from %d for unique id %llu",
    int(message->source_task()), uint64_t(message->unique_mpi_id()));

  ack_needed_t::iterator it =
    send_needs_rendezvous_ack_.find(message->unique_mpi_id());
  if (it == send_needs_rendezvous_ack_.end()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpi_queue[%s]::incoming_message: rendezvous ack with no match on %s",
                     taskid_.to_string().c_str(), message->to_string().c_str());
  }

  mpi_queue_send_request* req = it->second;
  send_needs_rendezvous_ack_.erase(it);
  message->rendezvous_ack_to_payload();

  /** the message either needs a nic ack or an mpi completion ack
   to finish everything off
   */
  if (message->protocol()->send_needs_completion_ack()) {
    send_needs_completion_ack_[message->unique_mpi_id()] = req;
    message->set_needs_ack(false);
  }
  else {
    send_needs_nic_ack_.push_back(req);
    message->set_needs_ack(true);
  }
  server_->send(message);
}

//
// Handle a new incoming message.  Can be either an eager send (with data)
// or a new handshake request
//
void
mpi_queue::incoming_new_message(const mpi_message::ptr& message)
{
  mpi_queue_debug("incoming new message %s", message->to_string().c_str());

  task_id tid = message->source_task();

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
  else if (message->ignore_seqnum()) {
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
mpi_queue::find_request(pending_message_t &pending,
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
mpi_queue::find_pending_request(const mpi_message::ptr& message,
                                bool set_need_recv)
{
  mpi_queue_recv_request* req = find_request(pending_message_, message);
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
mpi_queue::find_waiting_request(const mpi_message::ptr& message)
{
  mpi_queue_recv_request* req = find_request(waiting_message_, message);
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
  mpi_queue_debug("handle nic ack for message %s", message->to_string().c_str());

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

std::string
mpi_queue::id_string() const
{
  return taskid_.to_string();
}

#if SSTMAC_ENABLE_MPI_TIMELINE

template <class Combined, class Left, class Right>
Combined
pack_bits(Left l, Right r){
  return (((Combined)l) << sizeof(Right)*8) + r;
}

template <class Combined, class Left, class Right>
void
unpack_bits(Combined c, Left& l, Right& r){
  l = c >> sizeof(Right)*8;
  Combined xor_bitmask = ((Combined)l) << sizeof(Right)*8;
  r = c^xor_bitmask;
}

void
mpi_queue::log_completion(const mpi_message::ptr &m)
{
  message_log_data_t data;
  data.start = m ->start_time();
  data.finish = server_->now();
  data.bytes = m->payload_bytes();
  //I want global ranks
  uint64_t comm_and_rank = pack_bits<uint64_t>(int(m->commid()), int(m->source()));
  if (m->cat() == mpi_message::collective){
    collective_logs_[comm_and_rank].push_back(data);
  } else {
    pt2pt_logs_[comm_and_rank].push_back(data);
  }
}

void
mpi_queue::print_log(std::ostream& os, std::list<message_log_data_t>& theList, int src)
{
  std::list<message_log_data_t>::iterator it;
  for (it=theList.begin(); it != theList.end(); ++it){
    message_log_data_t& data = *it;
    double start = data.start.sec();
    double finish = data.finish.sec();
    double delta_t = finish - start;
    os << sprockit::printf("%5d %8.3fKB %8.4e %8.4e\n",
       src, (data.bytes / 1e3), start, finish);
  }
}

void
mpi_queue::print_log(source_to_log_map& theMap, const char* type)
{
  int dst = api_->comm_world()->rank();
  mpi_queue::source_to_log_map::iterator it;
  std::ofstream output;
  std::string fname = sprockit::printf("mpi_timeline.%d.%s", dst, type);
  output.open(fname.c_str());
  int lastComm = -1;
  for (it=theMap.begin(); it != theMap.end(); ++it){
    uint64_t comm_and_rank = it->first;
    int comm, rank;
    unpack_bits(comm_and_rank, comm, rank);
    if (comm != lastComm){
      lastComm = comm;
      if (comm != MPI_COMM_WORLD){
        output << sprockit::printf("MPI Comm %d\n", comm);
        mpi_comm* commPtr = api_->get_comm(comm);
        for (int i=0; i < commPtr->size(); ++i){
          output << sprockit::printf("Rank %d -> %d\n", i, int(commPtr->peer_task(mpi_id(i))));
        }
      } else {
        output << "MPI_COMM_WORLD" << std::endl;
      }
    }
    print_log(output, it->second, rank);
  }
  output.close();
}

void
mpi_queue::print_log()
{
  if (pt2pt_logs_.empty() && collective_logs_.empty())
    return;

  print_log(pt2pt_logs_, "pt2pt");
  print_log(collective_logs_, "collective");
  pt2pt_logs_.clear();
  collective_logs_.clear();
}
#endif

}
} // end of namespace sstmac

