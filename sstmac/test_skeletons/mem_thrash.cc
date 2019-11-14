#include <mpi.h>
#include <stddef.h>
#include <stdio.h>
#include <vector>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

#define sstmac_app_name mem_thrash

RegisterKeywords(
 { "sources", "ranks of senders" },
 { "destinations", "ranks of recvers" },
);

static void ping(int rank, int src, int dst, int msize) {
  if (rank == src) {
    MPI_Send(NULL, msize, MPI_INT, dst, 0, MPI_COMM_WORLD);
  } else if (rank == dst) {
    MPI_Recv(NULL, msize, MPI_INT, src, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }
}

void runMemThrash()
{
  int sizes[18] = {1,2,4,8,16,32,128,256,512,1024,2048,5096,10192,20384,40768,81536,163072,326144};

  int nrepeats = sstmac::getParam<int>("repeats", 10);
  for (int s : sizes) {
    sstmac_usleep(4);
    for (int r=0; r < nrepeats; ++r){
      sstmac_memread(s);
    }
  }
}

void runPingPong()
{
  int sizes[18] = {1,2,4,8,16,32,128,256,512,1024,2048,5096,10192,20384,40768,81536,163072,326144};

  std::vector<int> src = sstmac::getArrayParam<int>("sources");
  std::vector<int> dst = sstmac::getArrayParam<int>("destinations");

  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  for (int i=0; i < src.size(); ++i) {
    for (int s : sizes) {
      double begin,end;
      if (rank == src[i])
        begin = MPI_Wtime();
      ping(rank,src[i],dst[i],s);
      ping(rank,dst[i],src[i],s);
      if (rank == src[i]) {
        end = MPI_Wtime();
        double time = (end - begin);
        int bytes = s * sizeof(int);
        double bw = bytes / time / 1e9;
        printf("%i: %8.4f GB/s\n", bytes, bw);
      }
    }
  }
}

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);

  int rank; 
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  if (rank % 2 == 0){
    runPingPong();
  } else {
    runMemThrash();
  }

  MPI_Finalize();

  return 0;
}

