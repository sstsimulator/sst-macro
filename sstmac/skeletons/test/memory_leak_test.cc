#define sstmac_app_name memory_leak_test

#include <sstmac/compute.h>
#include <sstmac/replacements/mpi.h>
#include <sprockit/sim_parameters.h>
#include <math.h>

static int max_pt2pt_count = 64000;
static int max_all_count = 4096;


static void test_pt2pt(MPI_Comm comm);
static void test_allgather(MPI_Comm comm);

#define finish_test() \
  if (rank==0) printf("Rank %d on MPI_Comm %ld passed test: %8.2e\n", rank, comm, MPI_Wtime());

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  sprockit::sim_parameters* params = get_params();
  max_pt2pt_count = params->get_optional_int_param("max_pt2pt_count", max_pt2pt_count);
  max_all_count = params->get_optional_int_param("max_all_count", max_all_count);

  test_pt2pt(MPI_COMM_WORLD);
  test_allgather(MPI_COMM_WORLD);

  MPI_Finalize();
  return 0;
}

void
test_pt2pt(MPI_Comm comm)
{
  int rank, size;
  int worldRank;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);

  static const int num_sendrecvs = 4;
  MPI_Request reqs[8];
  int disp = 1;
  int count = max_pt2pt_count; 
  int tag = 100;
  int reqIdx = 0;
  for (int i=1; i < num_sendrecvs; ++i){
    disp = disp % size;
    int send_to = (rank + disp) % size;
    int recv_from = (rank + size - disp) % size;
    disp *= 2;
    MPI_Isend(NULL, count, MPI_INT, send_to, tag, comm, &reqs[reqIdx]);
    ++reqIdx;
    MPI_Irecv(NULL, count, MPI_INT, recv_from,
              tag, comm, &reqs[reqIdx]);
    ++reqIdx;
  }
  MPI_Waitall(reqIdx, reqs, MPI_STATUSES_IGNORE);
  finish_test();
}


void
test_allgather(MPI_Comm comm)
{
  int rank, size;
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &size);
  int count = max_all_count; 
  MPI_Allgather(NULL, count, MPI_INT, NULL, count, MPI_INT, comm);
  finish_test();
}







