#ifndef MPI_ROUND_BY_ROUND_COLLECTIVE_H
#define MPI_ROUND_BY_ROUND_COLLECTIVE_H

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_payload.h>

namespace sstmac {
namespace sw {

class mpi_round_by_round_collective :
  public mpi_collective
{
 public:

  virtual void
  send_complete(const mpi_message::ptr& msg);

  virtual void
  recv_complete(const mpi_message::ptr &msg);

  virtual const char*
  name() const = 0;

  virtual ~mpi_round_by_round_collective() throw() {}

  void start();

 protected:
  /// Hi.
  mpi_round_by_round_collective(mpi_request* when_complete,
                 mpi_queue* queue, mpi_tag tag,
                 mpi_comm* comm, event_handler* completion,
                 int sendcnt, int recvcnt,
                 mpi_type_id send_type,
                 mpi_type_id recv_type);

  void start_next_round();

  void start_send_round();

  void start_recv_round();

  virtual void
  finalize_buffer() = 0;

  virtual mpi_payload::ptr
  result_payload() = 0;

 protected:
  struct partner
  {
   mpi_id rank;
   int offset;
   int nelems;
   partner(){}
  };

  struct round
  {
    //the guy I'm sending to
    partner send_partner;
    partner recv_partner;
  };

  std::vector<round> rounds_;

  int current_send_round_number_;

  int current_recv_round_number_;

  char* buffer_;

  int sendcnt_;

  int recvcnt_;

  mpi_type* send_type_obj_;

  mpi_type* recv_type_obj_;

  payload::const_ptr input_payload_;

};

}
}



#endif // MPI_ROUND_BY_ROUND_COLLECTIVE_H
