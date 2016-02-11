#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_simultaneous_btree_scan.h>

#define mpi_scan_debug(...) \
  mpi_coll_debug("MPI_Scan binary tree algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

mpi_simultaneous_btree_scan_engine::mpi_simultaneous_btree_scan_engine(
  mpi_request* thekey,
  mpi_queue* queue,
  int count, mpi_type_id type,
  mpi_op* op,
  mpi_tag tag, mpi_comm* comm,
  const payload::const_ptr& content,
  event_handler* completion)
  : mpi_collective(thekey, queue, tag, comm, completion),
    count_(count), type_(type), op_(op), content_(content),
    send_round_(0), send_two_to_the_k_(1),
    recv_round_(0), recv_two_to_the_k_(1),
    me_(comm->my_task()),
    nproc_(comm->size()),
    pending_sends_(0), pending_recvs_(0),
    send_done_(false), recv_done_(false),
    in_send_recv_(false)
{
}

void
mpi_simultaneous_btree_scan_engine::next_send()
{
  int dst = me_ + send_two_to_the_k_;
  ++send_round_;
  send_two_to_the_k_ *= 2;
  if (dst < nproc_) {
    mpi_scan_debug("send to %d in round %d",
        dst, send_round_ - 1);

    ++pending_sends_;
    start_send(count_, type_, tag_, mpi_id(dst), content_);
  }
  else {
    mpi_scan_debug("send done");
    send_done_ = true;
  }
}

void
mpi_simultaneous_btree_scan_engine::next_recv()
{
  int src = me_ - recv_two_to_the_k_;
  ++recv_round_;
  recv_two_to_the_k_ *= 2;
  if (src >= 0) {
    ++pending_recvs_;
    mpi_scan_debug("recv from %d in round %d",
        src, recv_round_ - 1);
    start_recv(count_, type_, tag_, mpi_id(src));
  }
  else {
    //actually no pending recvs
    mpi_scan_debug("recv done");
    recv_done_ = true;
  }
}

void
mpi_simultaneous_btree_scan_engine::check_complete()
{
  if (pending_sends_ == 0 && pending_recvs_ == 0 && !in_send_recv_) {
    complete(content_);
  }  
}

void
mpi_simultaneous_btree_scan_engine::sendrecv()
{
  in_send_recv_ = true;
  next_send();
  next_recv();

  if (recv_done_) { //finish all my sends
    while (!send_done_) {
      next_send();
    }
  }
  
  in_send_recv_ = false;
  check_complete();
}

void
mpi_simultaneous_btree_scan_engine::send_complete(const mpi_message::ptr& msg)
{
  --pending_sends_;
  mpi_scan_debug("send complete to %d - now %d pending sends, %d pending recvs",
    int(msg->dest()), pending_sends_, pending_recvs_);

  check_complete();
}

void
mpi_simultaneous_btree_scan_engine::recv_complete(const mpi_message::ptr& msg)
{
  payload::const_ptr load = msg->content();
  if (load) {
    content_ = combine_content(content_, load, op_, comm_->rank_, msg->source());
  }

  //each recv triggers the next sendrecv
  sendrecv();

  --pending_recvs_;
  mpi_scan_debug("recv complete from %d - now %d pending sends, %d pending recvs",
    int(msg->source()), pending_sends_, pending_recvs_);

  check_complete();
}

void
mpi_simultaneous_btree_scan_engine::start()
{
  if (nproc_ == 1) {
    mpi_collective::complete(content_);
  }
  else {
    sendrecv();
  }
}

mpi_simultaneous_btree_scan_engine::~mpi_simultaneous_btree_scan_engine() throw()
{
}

std::string
mpi_simultaneous_btree_scan_engine::to_string() const
{
  return "mpi_scan";
}

}
}

