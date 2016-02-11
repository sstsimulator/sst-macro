#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sprockit/errors.h>
#include <sstmac/util.h>
#include <sstmac/replacements/mpi.h>


using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;

sstmac_register_app(test_new_collectives);

int
test_new_collectives_main(int argc, char **argv)
{
  MPI_Init(&argc, &argv);

  int rank, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  int my_send = rank;
  int* send_buf = &my_send;
  int* recv_buf = new int[nproc];
  ::memset(recv_buf, 0, nproc * sizeof(int));
  MPI_Allgather(send_buf, 1, MPI_INT, recv_buf, 1, MPI_INT, MPI_COMM_WORLD);

  for (int i=0; i < nproc; ++i){
    if (recv_buf[i] != i){
      spkt_throw_printf(sprockit::value_error,
        "buf[%d] != %d: %d\n",
        i, i, recv_buf[i]);
    }
  }

  MPI_Finalize();
  return 0;
}

