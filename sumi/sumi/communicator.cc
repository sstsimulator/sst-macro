#include <sumi/communicator.h>
#include <sumi/transport.h>
#include <sprockit/errors.h>

namespace sumi {

void
communicator::rank_resolved(int global_rank, int comm_rank)
{
  for (rank_callback* cback : rank_callbacks_){
    cback->rank_resolved(global_rank, comm_rank);
  }
}

global_communicator::global_communicator(transport *tport) :
  transport_(tport), communicator(tport->rank())
{
}

int
global_communicator::nproc() const
{
  return transport_->nproc();
}

int
global_communicator::comm_to_global_rank(int comm_rank) const
{
  return comm_rank;
}

int
global_communicator::global_to_comm_rank(int global_rank) const
{
  return global_rank;
}

int
index_communicator::global_to_comm_rank(int global_rank) const
{
  spkt_throw(sprockit::unimplemented_error,
    "index_domain::global_to_comm_rank: this should only be involved in failures");
}

}

