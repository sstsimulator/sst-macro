#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_round_by_round_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_payload.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_api.h>

namespace sstmac {
namespace sw {


mpi_round_by_round_collective::mpi_round_by_round_collective(
    mpi_request* when_complete,
    mpi_queue* queue,
    mpi_tag tag,
    mpi_comm* comm,
    event_handler* completion,
    int sendcnt, int recvcnt,
    mpi_type_id send_type,
    mpi_type_id recv_type)
  : mpi_collective(when_complete, queue, tag, comm, completion),
  current_send_round_number_(0),
  current_recv_round_number_(0),
  buffer_(0),
  sendcnt_(sendcnt),
  recvcnt_(recvcnt)
{
  send_type_ = send_type;
  recv_type_ = recv_type;
  send_type_obj_ = queue->api()->type_from_id(send_type);
  recv_type_obj_ = queue->api()->type_from_id(recv_type);
}

void
mpi_round_by_round_collective::start()
{
  start_next_round();
  mpi_collective::start();
}

void
mpi_round_by_round_collective::send_complete(const mpi_message::ptr& msg)
{
  int dest = msg->source(); //this comes from an ack, which is reversed - kind of confusing
#if SSTMAC_SANITY_CHECK
  round& r = rounds_[current_send_round_number_];
  if (r.send_partner.rank != dest){
    spkt_throw_printf(sprockit::illformed_error,
        "Rank %d round %d for collective %s - actual dest %d does not match %d on round %d for message %s",
        int(comm_->rank()),
        current_send_round_number_, name(), dest, 
        int(r.send_partner.rank), 
        current_send_round_number_,
        msg->to_string().c_str());
  }
#endif

  mpi_coll_debug("RxR collective",
    ": %s: finished sending for round=%d tag=%d to %d",
    name(), current_send_round_number_, int(tag_), dest);

  ++current_send_round_number_;
  start_next_round();
}

void
mpi_round_by_round_collective::recv_complete(const mpi_message::ptr& msg)
{
  round& r = rounds_[current_recv_round_number_];
#if SSTMAC_SANITY_CHECK
  if (r.recv_partner.rank != msg->source()){
    spkt_throw_printf(sprockit::illformed_error,
        "round %d for collective %s - actual source %d does not match %d",
        current_recv_round_number_, name(), int(msg->source()), int(r.recv_partner.rank));
  }
#endif

  mpi_coll_debug("RxR collective",
    ": %s: finished sending for round=%d tag=%d to %d",
    name(), current_recv_round_number_, int(tag_), int(msg->source()));
  
  ++current_recv_round_number_;
  start_next_round();
}

void
mpi_round_by_round_collective::start_send_round()
{
  round& r = rounds_[current_send_round_number_];
  partner& p = r.send_partner;
  mpi_coll_debug("RxR collective",
    ": %s: sending to %d nelems=%d offset=%d for round=%d tag=%d",
    name(), int(p.rank), p.nelems, p.offset, current_send_round_number_, int(tag_));


  if (buffer_){
    int byte_offset = p.offset*send_type_obj_->extent();
    start_send(p.nelems, p.rank, buffer_ + byte_offset);
  }  else {
    start_send(p.nelems, p.rank);
  }

}

void
mpi_round_by_round_collective::start_recv_round()
{
  round& r = rounds_[current_recv_round_number_];
  partner& p = r.recv_partner;
  mpi_coll_debug("RxR collective",
    ": %s: recving from %d nelems=%d offset=%d for round=%d tag=%d\n",
    name(), int(p.rank), p.nelems, p.offset, current_recv_round_number_, int(tag_));

  if (buffer_){
    int byte_offset = p.offset*recv_type_obj_->extent();
    start_recv(p.nelems, p.rank, buffer_ + byte_offset); 
  } else {
    start_recv(p.nelems, p.rank);
  }
}

void
mpi_round_by_round_collective::start_next_round()
{
  bool valid_round = current_send_round_number_ < int(rounds_.size());
  bool rounds_match = current_send_round_number_ == current_recv_round_number_;
  if (valid_round && rounds_match){
    //both send and recv for this round are done

    mpi_coll_debug("RxR collective",
     ": %s: starting next round=%d of %d on tag=%d ",
     name(),
     current_send_round_number_,
     rounds_.size(), int(tag_));

    start_send_round();
    start_recv_round();
  }
  else if (rounds_match && current_send_round_number_ == rounds_.size()){
    //oh, we're done
    if (buffer_){
      finalize_buffer();
      mpi_payload::ptr result = result_payload();
      mpi_collective_payload::ptr load = new mpi_collective_payload(1); //only a single entry for now
      load->set_content(result, 0);
      mpi_collective::complete(load);
    } else {
      mpi_collective::complete(payload::const_ptr());
    }
  }
  else {
    mpi_coll_debug("RxR collective",
     ": %s: waiting to start next round=%d,%d of %d on tag=%d ",
     name(),
     current_send_round_number_,
     current_recv_round_number_,
     rounds_.size(), int(tag_));
  }
}

}
}
