#include <sprockit/test/test.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/replacements/mpi.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/skeleton.h>

#define sstmac_app_name mpi_topology

using namespace sstmac;
using namespace sstmac::sw;
using namespace sstmac::hw;

void sleep(int seconds)
{
  thread* thr = operating_system::current_thread();
  app* app = safe_cast(sstmac::sw::app, thr);
  app->sleep(timestamp(seconds));
}

void do_sendrecv(traffic_pattern::type_t ty)
{
  int tag = 101;
  std::vector<node_id> node_partners;
  sumi::mpi_api* mpi = current_mpi();
  topology::global()->send_partners(
    ty,
    mpi->my_addr(),
    node_partners);
  int num_partners = node_partners.size();
  std::vector<int> send_partners(num_partners, 0);
  for (int i=0; i < node_partners.size(); ++i) {
    send_partners[i] = mpi->get_partner(node_partners[i]);
  }

  MPI_Request send_reqs[100];
  MPI_Request recv_reqs[100];
  MPI_Status recv_stats[100];
  for (int i=0; i < num_partners; ++i) {
    MPI_Isend(0, 1000, MPI_INT, send_partners[i], tag, MPI_COMM_WORLD,
              &send_reqs[i]);
    MPI_Irecv(0, 1000, MPI_INT, MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, &recv_reqs[i]);
  }
  MPI_Waitall(num_partners, send_reqs, MPI_STATUSES_IGNORE);
  MPI_Waitall(num_partners, recv_reqs, recv_stats);

  node_partners.clear();
  std::vector<int> recv_partners(num_partners, 0);
  std::vector<int> actual_recvs(num_partners, 0);
  topology::global()->recv_partners(
    ty,
    mpi->my_addr(),
    node_partners);
  for (int i=0; i < num_partners; ++i) {
    recv_partners[i] = mpi->get_partner(node_partners[i]);
    actual_recvs[i] = recv_stats[i].MPI_SOURCE;
  }

  UnitTest unit;

  // the messages we received should match the recv partners
  // let's sort both, though
  std::sort(recv_partners.begin(), recv_partners.end());
  std::sort(actual_recvs.begin(), actual_recvs.end());

  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  for (int i=0; i < nproc; ++i) {
    MPI_Barrier(MPI_COMM_WORLD);
    if (i == me) {
      if (me == 0) {
        coutn << sprockit::printf("Testing %s\n", traffic_pattern::tostr(ty));
      }
      for (int j=0; j < num_partners; ++j) {
        if (recv_partners[j] == actual_recvs[j]) {
          coutn << sprockit::printf("PASSED: MPI rank %d partner %d matches: %d\n",
                                 me, j, recv_partners[j]);
        }
        else {
          coutn <<
                    sprockit::printf("FAILED: MPI rank %d at addr %ld partner %d does not match: "
                              "%d != %d\n",
                              me, long(mpi->my_addr()), j,
                              recv_partners[j], actual_recvs[j]);
        }
      }
    }
  }
}

int USER_MAIN(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  do_sendrecv(traffic_pattern::nearest_neighbor);
  do_sendrecv(traffic_pattern::bit_complement);
  do_sendrecv(traffic_pattern::tornado);
  MPI_Finalize();
  return 0;
}


