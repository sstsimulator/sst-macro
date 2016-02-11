#ifndef MPI_BRUCK_ALLGATHER_H
#define MPI_BRUCK_ALLGATHER_H

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_round_by_round_collective.h>

namespace sstmac {
namespace sw {

class mpi_bruck_allgather_engine :
  public mpi_round_by_round_collective

{
 public:
  std::string
  to_string() const {
    return "bruck MPI allgather";
  }

  const char*
  name() const {
    return "MPI_Allgather";
  }

  mpi_bruck_allgather_engine(mpi_request* thekey,
                     mpi_queue* queue,
                     int sendcnt, mpi_type_id sendtype,
                     int recvcnt, mpi_type_id recvtype,
                     mpi_tag tag, mpi_comm* comm,
                     const payload::const_ptr& content,
                     event_handler* completion);

 protected:
  void finalize_buffer();

  mpi_payload::ptr
  result_payload();

};


}
}

#endif // MPI_BRUCK_ALLGATHER_H
