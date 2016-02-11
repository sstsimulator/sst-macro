#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_logtwo_virt.h>

#define mpi_allreduce_debug(...) \
  mpi_coll_debug("MPI_Allreduce log2 algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

int
mpi_logtwo_virt_allreduce::two_to_power(int n)
{
  int p2 = 1;
  for (int i=0; i < n; ++i) {
    p2 *= 2;
  }
  return p2;
}

int mpi_logtwo_virt_allreduce::log_base_two(int n)
{
  int log2 = 0;
  int remainder = n;
  while (remainder > 1) {
    ++log2;
    remainder /= 2;
  }
  return log2;
}

void
mpi_logtwo_virt_allreduce::configure_round(int me, int round,
    int& count, int& partner)
{
  int delta = two_to_power(round);
  int group_mod_divider = delta * 2;
  int my_group_rank = me % group_mod_divider;
  count = count_ / group_mod_divider;

  bool power_bottom = my_group_rank < delta;
  if (power_bottom) {
    partner = me + delta;
  }
  else {
    partner = me - delta;
  }
}

int
mpi_logtwo_virt_allreduce::virtual_rank_to_actual_rank(int rank)
{
  if (rank < nproc_) {
    return rank;
  }
  //I exceed max by
  int max_excess = rank - nproc_ + 1;

  int delta_exchange = 1;
  while (delta_exchange < max_excess) {
    delta_exchange *= 2;
  }

  //figure out the first round something real tries to exchange with this
  int partner = nproc_;
  int count = 0;
  int round = 0;
  while (partner >= nproc_) {
    configure_round(rank, round, count, partner);
    ++round;
  }

  return partner;
}

void
mpi_logtwo_virt_allreduce::do_run_round(int round, int me, int virtual_me,
                                        std::list<exchange>& to_do)
{
  mpi_allreduce_debug("round %d for virtual rank %d", round, virtual_me);
  reducing_round_ = round < (num_rounds_ / 2);
  int effective_round = reducing_round_ ? round : num_rounds_ - round - 1;
  int virtual_partner, count;
  configure_round(virtual_me, effective_round, count, virtual_partner);
  int partner = virtual_partner;
  if (partner >= nproc_) {
    partner = virtual_rank_to_actual_rank(partner);
  }

  // you don't need to map two virtual nodes to each other
  if (virtual_me >= nproc_ && virtual_partner >= nproc_) {
  }
  else if (partner == me) {
    //make sure I take on this role going forward
    my_roles_.insert(virtual_partner);
    mpi_allreduce_debug("self-exchange %d<->%d (%d<->%d) in round %d count %d",
           virtual_me, virtual_partner, me, partner, round, count);
  }
  else {
    ++pending_sends_;
    ++pending_recvs_;
    to_do.push_back(exchange(partner,count));
    mpi_allreduce_debug("exchange %d<->%d (%d<->%d) in round %d count %d",
           virtual_me, virtual_partner, me, partner, round, count);
  }
}

void
mpi_logtwo_virt_allreduce::run_next_round()
{
  mpi_allreduce_debug("running round %d on node %d with %d virtual roles",
         round_, my_rank_, int(my_roles_.size()));

  std::set<int> current_roles = my_roles_;
  std::set<int>::iterator it, end = current_roles.end();
  std::list<exchange> to_do;
  for (it=current_roles.begin(); it != end; ++it) {
    int virtual_me = *it;
    do_run_round(round_, my_rank_, virtual_me, to_do);
  }

  if (to_do.size() == 0) {
    //we didn't actually do anything
    ++round_;
    if (round_ == num_rounds_) {
      mpi_collective::complete(payload::const_ptr());
    }
    else {
      run_next_round();
    }
  }
  else {
    std::list<exchange>::iterator it, end = to_do.end();
    for (it=to_do.begin(); it != end; ++it) {
      exchange& e = *it;
      start_send(e.count, type_, tag_, mpi_id(e.partner));
      start_recv(e.count, type_, tag_, mpi_id(e.partner));
    }
  }
}

void
mpi_logtwo_virt_allreduce::send_complete(const mpi_message::ptr& msg)
{
  --pending_sends_;
  if (round_ == num_rounds_ && pending_sends_ == 0) {
    mpi_collective::complete(payload::const_ptr());
  }
}

void
mpi_logtwo_virt_allreduce::recv_complete(const mpi_message::ptr& msg)
{
  --pending_recvs_;
  if (reducing_round_) {
    //nothing - payloads not yet supported
  }

  if (pending_recvs_ == 0) {
    //this round of recvs is complete
    ++round_;
    if (round_ == num_rounds_) {
      if (pending_sends_ == 0) {
        mpi_collective::complete(payload::const_ptr());
      }
    }
    else {
      run_next_round();
    }
  }


}

mpi_logtwo_virt_allreduce::mpi_logtwo_virt_allreduce(
  mpi_request* thekey,
  mpi_queue* queue,
  int count, mpi_type_id datatype,
  mpi_op* op, mpi_id root,
  mpi_tag tag, mpi_comm* comm,
  const payload::const_ptr& content,
  event_handler* completion)
  : mpi_collective(thekey, queue, tag, comm, completion),
    count_(count), type_(datatype),
    op_(op), root_(root),
    pending_sends_(0),
    pending_recvs_(0),
    reducing_round_(false),
    complete_lock_(false),
    nproc_(comm->size()),
    my_rank_(comm->my_task()),
    round_(0),
    num_rounds_(0)
{
  num_rounds_ = log_base_two(nproc_);
  int proc_test = two_to_power(num_rounds_);
  if (nproc_ != proc_test) { //we don't have even power of two
    ++num_rounds_;
  }

  //we need to fan out and fan back in
  num_rounds_ *= 2;
  my_roles_.insert(my_rank_);
}

void
mpi_logtwo_virt_allreduce::start()
{
  run_next_round();
}

}
}


