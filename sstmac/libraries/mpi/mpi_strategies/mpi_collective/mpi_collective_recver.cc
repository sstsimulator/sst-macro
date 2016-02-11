#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_recver.h>

namespace sstmac {
namespace sw {

void
mpi_collective::recver::handle(const sst_message::ptr& msg)
{
  mpi_message::ptr mmsg = ptr_safe_cast(mpi_message, msg);
  parent_->recv_complete(mmsg);
}

}
}
