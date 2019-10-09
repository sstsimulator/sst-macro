#include <mpi.h>
#include <stddef.h>
#include <stdio.h>
#include <vector>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name mpi_ping_pong

RegisterKeywords(
 { "sources", "ranks of senders" },
 { "destinations", "ranks of recvers" },
);

static void sendrecv(int rank, int src, int dst, int msize) {
  if (rank == src) {
    MPI_Send(NULL, msize, MPI_INT, dst, 0, MPI_COMM_WORLD);
  } else if (rank == dst) {
    MPI_Recv(NULL, msize, MPI_INT, src, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }
}

int USER_MAIN(int argc, char** argv)
{
  int sizes[18] = {1,2,4,8,16,32,128,256,512,1024,2048,5096,10192,20384,40768,81536,163072,326144};
 
  std::vector<int> src = sstmac::getArrayParam<int>("sources");
  std::vector<int> dst = sstmac::getArrayParam<int>("destinations");
  bool pong = sstmac::getParam<bool>("pong", true);
  bool ping = sstmac::getParam<bool>("ping", true);
  bool barrier = sstmac::getParam<bool>("barrier", true);

  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  for (int i=0; i < src.size(); ++i) {
    if (rank == 0)
      printf("ping-pong between %i and %i\n",src[i],dst[i]);
    for (int s : sizes) { 
      double begin,end;
      if (barrier) MPI_Barrier(MPI_COMM_WORLD);
      if (rank == src[i])
        begin = MPI_Wtime();
      if (ping) sendrecv(rank,src[i],dst[i],s);
      if (pong) sendrecv(rank,dst[i],src[i],s);
      if (rank == src[i]) {
        end = MPI_Wtime();
        double time = (end - begin);
        int bytes = s * sizeof(int);
        double bw = bytes / time / 1e9;
        printf("%i: %8.4f GB/s\n", bytes, bw);
      }
    }
  }

  MPI_Finalize();

  return 0;
}
