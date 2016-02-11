#include "sendrecv.h"

namespace sendrecv
{
  SpktRegisterApp("sendrecv", sendrecv_skeleton);

  void
  sendrecv_skeleton::skeleton_main()
  {
    int my_argc = argc();
    char** my_argv = argv();

    int tag(0);
    int dst(1);
    int src(0);

    MPI_Init(NULL, NULL);
    int me, nproc;
    MPI_Comm_rank(MPI_COMM_WORLD, &me);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);

    if (me == 0 && nproc != 2)
      throw sprockit::input_error("sendrecv should only be run with two processors");

    if (me == 0)
    {
      MPI_Send(NULL, message_size_, MPI_INT, dst, tag, MPI_COMM_WORLD);
      std::cout << "rank " << me << " sending a message\n";
    }
    else
    {
      MPI_Status stat;
      MPI_Recv(NULL, message_size_, MPI_INT, src, tag, MPI_COMM_WORLD, &stat);
      std::cout << "rank " << me << " receiving a message\n";
    }
    MPI_Finalize();
  }

} //end namespace sendrecv


