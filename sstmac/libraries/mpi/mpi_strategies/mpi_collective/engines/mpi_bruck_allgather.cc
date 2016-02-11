#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_bruck_allgather.h>

namespace sstmac {
namespace sw {

mpi_bruck_allgather_engine::mpi_bruck_allgather_engine(
    mpi_request* thekey,
    mpi_queue* queue,
    int sendcnt, mpi_type_id sendtype,
    int recvcnt, mpi_type_id recvtype,
    mpi_tag tag, mpi_comm* comm,
    const payload::const_ptr& content,
    event_handler* completion) :
  mpi_round_by_round_collective(thekey, queue, tag, comm, completion, sendcnt, recvcnt, sendtype, recvtype)
{
  int nproc = comm->size();
  int me = comm->rank();

  if (content){
    long payload_size = content->byte_length();
    long buffer_size = payload_size * int(comm->size());
    buffer_ = new char[buffer_size];
    //the values end up rotated/scrambled
    long my_offset = 0; //payload_size * int(comm->rank());
    char* my_data = buffer_ + my_offset;
    ::memset(buffer_, 0, buffer_size);
    ::memcpy(my_data, content->data(), payload_size);
  }

  /** let's go bruck algorithm for now */
  int _nproc = 1;
  int log2nproc = 0;
  while (_nproc < nproc)
  {
    ++log2nproc;
    _nproc *= 2;
  }

  int num_extra_procs = 0;
  if (_nproc > nproc){
    --log2nproc;
    //we will have to do an extra exchange in the last round
    num_extra_procs = nproc - _nproc / 2;
  }

  int num_rounds = log2nproc;

  //in the last round, we send half of total data to nearest neighbor
  //in the penultimate round, we send 1/4 data to neighbor at distance=2
  //and so on...
  int total_rounds = num_rounds + (num_extra_procs ? 1 : 0);
  rounds_.resize(total_rounds);
  int partner_gap = 1;
  int round_nelems = sendcnt;
  for (int i=0; i < num_rounds; ++i){
    round round;
    round.send_partner.rank = mpi_id((me + nproc - partner_gap) % nproc);
    round.recv_partner.rank = mpi_id((me + partner_gap) % nproc);
    round.send_partner.offset = 0;
    round.recv_partner.offset = round_nelems;
    round.send_partner.nelems = round_nelems;
    round.recv_partner.nelems = round_nelems;
    rounds_[i] = round;
    partner_gap *= 2;
    round_nelems *= 2;
  }

  if (num_extra_procs){
    int nelems_extra_round = num_extra_procs * sendcnt;
    round round;
    round.send_partner.rank = mpi_id((me + nproc - partner_gap) % nproc);
    round.recv_partner.rank = mpi_id((me + partner_gap) % nproc);
    round.send_partner.offset = 0;
    round.recv_partner.offset = round_nelems;
    round.send_partner.nelems = nelems_extra_round;
    round.recv_partner.nelems = nelems_extra_round;
    rounds_[num_rounds] = round;
  }
}

void
mpi_bruck_allgather_engine::finalize_buffer()
{
  if (comm_->rank() == 0 || buffer_ == 0)
    return; //this is already in the right order - or no buffer


  //we need to reorder things a bit
  //first, copy everything out
  int nproc = comm_->size();
  int my_rank = comm_->rank();
  int type_size = recv_type_obj_->extent();
  int total_size = recvcnt_ * type_size * nproc;
  char* tmp = new char[total_size];
  ::memcpy(tmp, buffer_, total_size);

  for (int rank=0; rank < nproc; ++rank){
    //this rank is shifted to the wrong position
    int src_rank_offset = (rank + nproc - my_rank) % nproc;
    int src_buffer_offset = src_rank_offset * recvcnt_ * type_size;
    int dst_buffer_offset = rank * recvcnt_ * type_size;
    void* src = tmp + src_buffer_offset;
    void* dst = ((char*)buffer_) + dst_buffer_offset;
    ::memcpy(dst, src, recvcnt_ * type_size);
  }

  delete[] tmp;
}

mpi_payload::ptr
mpi_bruck_allgather_engine::result_payload()
{
  return new mpi_payload(buffer_, recv_type_obj_, recvcnt_ * comm_->size());
}

}
}
