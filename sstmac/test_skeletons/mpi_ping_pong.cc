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

void ping(int rank, int src, int dst, int msize) {
  if (rank == src) {
    MPI_Send(NULL, msize, MPI_INT, dst, 0, MPI_COMM_WORLD);
  } else if (rank == dst) {
    MPI_Recv(NULL, msize, MPI_INT, src, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }
}

int main(int argc, char** argv)
{
  int sizes[18] = {1,2,4,8,16,32,128,256,512,1024,2048,5096,10192,20384,40768,81536,163072,326144};
  SST::Params params = getParams();
  //params->parse_file(, false, true);
 
  std::vector<int> src;
  std::vector<int> dst;
  params.find_array("sources", src);
  params.find_array("destinations", dst);

  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  for (int i=0; i < src.size(); ++i) {
    if (rank == 0)
      printf("ping-pong between %i and %i\n",src[i],dst[i]);
    for (int s : sizes) { 
      double begin,end;
      MPI_Barrier(MPI_COMM_WORLD);
      if (rank == src[i])
        begin = MPI_Wtime();
      ping(rank,src[i],dst[i],s);
      ping(rank,dst[i],src[i],s);
      if (rank == src[i]) {
        end = MPI_Wtime();
        double time = (end - begin)*10e3;
        printf("%i: %8.4f ms\n", int(s * sizeof(int)), time);
      }
    }
  }

  MPI_Finalize();

  return 0;
}
