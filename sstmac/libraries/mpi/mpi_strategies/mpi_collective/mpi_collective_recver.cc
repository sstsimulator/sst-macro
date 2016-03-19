#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_recver.h>
#include <sprockit/util.h>

namespace sstmac {
namespace sw {

void
mpi_collective::recver::handle(event* ev)
{
  mpi_message* mmsg = safe_cast(mpi_message, ev);
  parent_->recv_complete(mmsg);
}

}
}
