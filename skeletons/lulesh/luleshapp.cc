/** \file
 * Hello world example app.
 * A very simple mpi skeleton application.
 */

#include "luleshapp.h"
#include <math.h>
#include <stdio.h>
#include <mpi.h>

using namespace sstmac::sw;

namespace luleshmodel
{
  SpktRegister("lulesh", app, luleshapp);

  // Overrides app::skeleton_main virtual function
  // The task thread context is started by a call into this function
  void
  luleshapp::skeleton_main()
  {
    sprockit::sim_parameters* params = get_params();

    int argc = 0; char** argv = 0;
    MPI_Init(&argc, &argv);
    double start(MPI_Wtime());
    world_ = MPI_COMM_WORLD;
    int rank; 
    MPI_Comm_rank(world_, &rank);

    int npes;
    MPI_Comm_size(world_, &npes);

    // calculate processor decomposition
    int pe_x = (int) cbrt(npes);
    int pe_y = pe_x;
    int pe_z = pe_x;

    if (npes != pe_x * pe_y * pe_z)
    {
      spkt_throw_printf(sprockit::spkt_error, "lulesh: needs a perfect cube of processors to run correctly");
    }

    // calculate data layout
    int nx = params->get_int_param("lulesh_nx");
    int ny = params->get_int_param("lulesh_ny");
    int nz = params->get_int_param("lulesh_nz");

    int iter_max = params->get_int_param("lulesh_iter");

    if (rank == 0)
    {
      //SSTMAC_DEBUG << "------------------------------------------------------------\n";
      //SSTMAC_DEBUG << "Sandia Nat. Labs LULESH Hydro. SSTMacro Model.\n";
      //SSTMAC_DEBUG << "Computer Science Research Institute, NM\n";
      //SSTMAC_DEBUG << "------------------------------------------------------------\n";
      //SSTMAC_DEBUG << "\n";
      //SSTMAC_DEBUG << "** DEVELOPMENTAL DO NOT USE FOR PRODUCTION **\n";
      //SSTMAC_DEBUG << "\n";
      //SSTMAC_DEBUG << "This is an early unvalidated simulation model.\n";
      //SSTMAC_DEBUG << "\n";
      //SSTMAC_DEBUG << "------------------------------------------------------------\n";
    }

    if (rank == 0)
    {
      //SSTMAC_DEBUG << "Simulation Information:\n";
      //SSTMAC_DEBUG << " -> NPEs (Total):     " << npes << "\n";
      //SSTMAC_DEBUG << " -> PE(x):            " << pe_x << "\n";
      //SSTMAC_DEBUG << " -> PE(y):            " << pe_y << "\n";
      //SSTMAC_DEBUG << " -> PE(z):            " << pe_z << "\n";
      //SSTMAC_DEBUG << " -> N(x) (Subdomain): " << nx << "\n";
      //SSTMAC_DEBUG << " -> N(y) (Subdomain): " << ny << "\n";
      //SSTMAC_DEBUG << " -> N(z) (Subdomain): " << nz << "\n";
      //SSTMAC_DEBUG << " -> Iter (Max):       " << iter_max << "\n";
    }

    double start_main = MPI_Wtime();
    application_main(iter_max, nx * ny * nz, (nx + 1) * (ny + 1) + (nz + 1),
        pe_x, nx);
    double end_main = MPI_Wtime();


    double done(MPI_Wtime());
  }

  void
  luleshapp::comm_recv(int numElem, int numNodes, int px, int nx)
  {
    int rank;
    int size;
    MPI_Comm_rank(world_, &rank);
    MPI_Comm_size(world_, &size);

    int x_left, x_right, y_left, y_right, z_up, z_down;
    int edge_a, edge_b, edge_c, edge_d, edge_e, edge_f, edge_g, edge_h;
    int edge_i, edge_j, edge_k, edge_l;
    int corner_a, corner_b, corner_c, corner_d;
    int corner_e, corner_f, corner_g, corner_h;
    int message_count = 0;

    // x-dim, comm left
    if (rank % px != 0)
    {
      x_left = rank - 1;
    }
    else
      x_left = -1;

    // x-dim, comm right
    if ((rank + 1) % px != 0)
    {
      x_right = rank + 1;
    }
    else
      x_right = -1;

    // y-dim, comm left
    const int level = (int) (rank / (px * px));
    if (rank < ((level + 1) * (px * px) - px))
    {
      y_left = rank + px;
    }
    else
      y_left = -1;

    // y-dim, comm right
    if (rank >= (level * (px * px) + px))
    {
      y_right = rank - px;
    }
    else
      y_right = -1;

    // z-dim, comm down
    if (rank < size - (px * px))
    {
      z_down = rank + (px * px);
    }
    else
      z_down = -1;

    // z-dim, comm up
    if (rank >= (px * px))
    {
      z_up = rank - (px * px);
    }
    else
      z_up = -1;

    //////////////////////////////////////////////////////////////////////////////////////

    // edge_a
    if (y_right > -1 && z_down > -1)
    {
      edge_a = z_down - px;
    }
    else
      edge_a = -1;

    // edge_b
    if (x_left > -1 && y_right > -1)
    {
      edge_b = x_left - px;
    }
    else
      edge_b = -1;

    // edge_c
    if (y_right > -1 && z_up > -1)
    {
      edge_c = z_up - px;
    }
    else
      edge_c = -1;

    // edge_d
    if (x_right > -1 && y_right > -1)
    {
      edge_d = y_right + 1;
    }
    else
      edge_d = -1;

    // edge_e
    if (x_left > -1 && z_down > -1)
    {
      edge_e = z_down - 1;
    }
    else
      edge_e = -1;

    // edge_f
    if (x_left > -1 && z_up > -1)
    {
      edge_f = z_up - 1;
    }
    else
      edge_f = -1;

    // edge_g
    if (x_right > -1 && z_up > -1)
    {
      edge_g = z_up + 1;
    }
    else
      edge_g = -1;

    // edge_h
    if (x_right > -1 && z_down > -1)
    {
      edge_h = z_down + 1;
    }
    else
      edge_h = -1;

    // edge_i
    if (y_left > -1 && z_up > -1)
    {
      edge_i = z_up + px;
    }
    else
      edge_i = -1;

    // edge_j
    if (y_left > -1 && x_left > -1)
    {
      edge_j = x_left + px;
    }
    else
      edge_j = -1;

    // edge_k
    if (y_left > -1 && z_down > -1)
    {
      edge_k = z_down + px;
    }
    else
      edge_k = -1;

    // edge_l
    if (x_right > -1 && y_left > -1)
    {
      edge_l = x_right + px;
    }
    else
      edge_l = -1;

    // corner_a
    if (edge_b > -1 && z_up > -1)
    {
      corner_a = edge_b - px * px;
    }
    else
      corner_a = -1;

    if (edge_d > -1 && z_up > -1)
    {
      corner_b = edge_d - px * px;
    }
    else
      corner_b = -1;

    if (edge_b > -1 && z_down > -1)
    {
      corner_c = edge_b + px * px;
    }
    else
      corner_c = -1;

    if (edge_d > -1 && z_down > -1)
    {
      corner_d = edge_d + px * px;
    }
    else
      corner_d = -1;

    if (edge_j > -1 && z_up > -1)
    {
      corner_e = edge_j - px * px;
    }
    else
      corner_e = -1;

    if (edge_l > -1 && z_up > -1)
    {
      corner_f = edge_l - px * px;
    }
    else
      corner_f = -1;

    if (edge_j > -1 && z_down > -1)
    {
      corner_g = edge_j + px * px;
    }
    else
      corner_g = -1;

    if (edge_l > -1 && z_down > -1)
    {
      corner_h = edge_l + px * px;
    }
    else
      corner_h = -1;

    //////////////////////////////////////////////////////////////////////////////////////

    int tag(0);
    int x_left_id(x_left % size);
    int x_right_id(x_right % size);
    int y_left_id(y_left % size);
    int y_right_id(y_right % size);
    int z_down_id(z_down % size);
    int z_up_id(z_up % size);
    int edge_a_id(edge_a % size);
    int edge_b_id(edge_b % size);
    int edge_c_id(edge_c % size);
    int edge_d_id(edge_d % size);
    int edge_e_id(edge_e % size);
    int edge_f_id(edge_f % size);
    int edge_g_id(edge_g % size);
    int edge_h_id(edge_h % size);
    int edge_i_id(edge_i % size);
    int edge_j_id(edge_j % size);
    int edge_k_id(edge_k % size);
    int edge_l_id(edge_l % size);
    int corner_a_id(corner_a % size);
    int corner_b_id(corner_b % size);
    int corner_c_id(corner_c % size);
    int corner_d_id(corner_d % size);
    int corner_e_id(corner_e % size);
    int corner_f_id(corner_f % size);
    int corner_g_id(corner_g % size);
    int corner_h_id(corner_h % size);

    /*//SSTMAC_DEBUG << "rank=%d, x-left=%d, x-right=%d, y-left=%d, y-right=%d, z-down=%d, z-up=%d, px=%d,",
     rank, x_left, x_right, y_left, y_right, z_down, z_up, px;
     //SSTMAC_DEBUG << "rank=%d, a=%d, b=%d, c=%d, d=%d, e=%d, f=%d, g=%d, h=%d, i=%d, j=%d, k=%d, l=%d\n",
     rank, edge_a, edge_b, edge_c, edge_d, edge_e, edge_f, edge_g, edge_h, edge_i,
     edge_j, edge_k, edge_l;*/
    /*printf("rank=%d, x-left=%d, x-right=%d, y-left=%d, y-right=%d, z-down=%d, z-up=%d, px=%d,",
     rank, x_left, x_right, y_left, y_right, z_down, z_up, px);
     printf("rank=%d, a=%d, b=%d, c=%d, d=%d, e=%d, f=%d, g=%d, h=%d, i=%d, j=%d, k=%d, l=%d\n",
     rank, edge_a, edge_b, edge_c, edge_d, edge_e, edge_f, edge_g, edge_h, edge_i,
     edge_j, edge_k, edge_l);
     printf("rank=%d, c_a=%d, c_b=%d, c_c=%d, c_d=%d, c_e=%d, c_f=%d, c_g=%d, c_h=%d\n",
     corner_a, corner_b, corner_c, corner_d, corner_e, corner_g, corner_h);*/

    int next_recv = 0;

    // 3 is the default setting for xfer-fields.
    const int plane_message_size = nx * nx * 3;
    const int pencil_message_size = nx * 3;
    const int corner_message_size = 3;

    if (x_left > -1)
    {
      MPI_Irecv(NULL, plane_message_size, MPI_DOUBLE,
          x_left_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted x_left recv to " << x_left_id << "\n";
    }

    if (x_right > -1)
    {
      MPI_Irecv(NULL, plane_message_size, MPI_DOUBLE,
          x_right_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted x_right recv to "  << x_right_id << "\n";
    }

    if (y_left > -1)
    {
      MPI_Irecv(NULL, plane_message_size, MPI_DOUBLE,
          y_left_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted y_left recv to " << y_left_id << "\n";
    }

    if (y_right > -1)
    {
      MPI_Irecv(NULL, plane_message_size, MPI_DOUBLE,
          y_right_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted x_right recv to " << x_right_id << "\n";
    }

    if (z_down > -1)
    {
      MPI_Irecv(NULL, plane_message_size, MPI_DOUBLE,
          z_down_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted z_down recv to " << z_down_id << "\n";
    }

    if (z_up > -1)
    {
      MPI_Irecv(NULL, plane_message_size, MPI_DOUBLE,
          z_up_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted z_up recv to " << z_up_id << "\n";
    }

    if (edge_a > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_a_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_a recv to " << edge_a_id << "\n";
    }

    if (edge_b > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_b_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_b recv to " << edge_b_id << "\n";
    }

    if (edge_c > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_c_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_c recv to " << edge_c_id << "\n";
    }

    if (edge_d > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_d_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_d recv to " << edge_d_id << "\n";
    }

    if (edge_e > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_e_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_e recv to " << edge_e_id << "\n";
    }

    if (edge_f > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_f_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_f recv to " << edge_f_id << "\n";
    }

    if (edge_g > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_g_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_g recv to " << edge_g_id << "\n";
    }

    if (edge_h > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_h_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_h recv to " << edge_h_id << "\n";
    }

    if (edge_i > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_i_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_i recv to " << edge_i_id << "\n";
    }

    if (edge_j > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_j_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_j recv to " << edge_j_id << "\n";
    }

    if (edge_k > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_k_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_k recv to " << edge_k_id << "\n";
    }

    if (edge_l > -1)
    {
      MPI_Irecv(NULL, pencil_message_size, MPI_DOUBLE,
          edge_l_id, tag, world_, &reqs[next_recv++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_l recv to " << edge_l_id << "\n";
    }

    if (corner_a > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_a_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_b > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_b_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_c > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_c_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_d > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_d_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_e > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_e_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_f > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_f_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_g > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_g_id, tag, world_, &reqs[next_recv++]);
    }

    if (corner_h > -1)
    {
      MPI_Irecv(NULL, corner_message_size, MPI_DOUBLE,
          corner_h_id, tag, world_, &reqs[next_recv++]);
    }

    //printf("rank=%d, posted %d receives\n", rank, next_recv);

  }

  void
  luleshapp::comm_send(int numElem, int numNodes, int px, int nx)
  {
    int rank, size;
    MPI_Comm_rank(world_, &rank);
    MPI_Comm_size(world_, &size);

    int x_left, x_right, y_left, y_right, z_up, z_down;
    int edge_a, edge_b, edge_c, edge_d, edge_e, edge_f, edge_g, edge_h;
    int edge_i, edge_j, edge_k, edge_l;
    int corner_a, corner_b, corner_c, corner_d;
    int corner_e, corner_f, corner_g, corner_h;
    int message_count = 0;

    // x-dim, comm left
    if (rank % px != 0)
    {
      x_left = rank - 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find x_left\n";
    }
    else
      x_left = -1;

    // x-dim, comm right
    if ((rank + 1) % px != 0)
    {
      x_right = rank + 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find x_right\n";
    }
    else
      x_right = -1;

    // y-dim, comm left
    const int level = (int) (rank / (px * px));
    if (rank < ((level + 1) * (px * px) - px))
    {
      y_left = rank + px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find y_left\n";
    }
    else
      y_left = -1;

    // y-dim, comm right
    if (rank >= (level * (px * px) + px))
    {
      y_right = rank - px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find y_right\n";
    }
    else
      y_right = -1;

    // z-dim, comm down
    if (rank < size - (px * px))
    {
      z_down = rank + (px * px);
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find z_downl\n";
    }
    else
      z_down = -1;

    // z-dim, comm up
    if (rank >= (px * px))
    {
      z_up = rank - (px * px);
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find z_up\n";
    }
    else
      z_up = -1;

    //////////////////////////////////////////////////////////////////////////////////////

    // edge_a
    if (y_right > -1 && z_down > -1)
    {
      edge_a = z_down - px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_a\n";
    }
    else
      edge_a = -1;

    // edge_b
    if (x_left > -1 && y_right > -1)
    {
      edge_b = x_left - px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_b\n";
    }
    else
      edge_b = -1;

    // edge_c
    if (y_right > -1 && z_up > -1)
    {
      edge_c = z_up - px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_c\n";
    }
    else
      edge_c = -1;

    // edge_d
    if (x_right > -1 && y_right > -1)
    {
      edge_d = y_right + 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_d\n";
    }
    else
      edge_d = -1;

    // edge_e
    if (x_left > -1 && z_down > -1)
    {
      edge_e = z_down - 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_e\n";
    }
    else
      edge_e = -1;

    // edge_f
    if (x_left > -1 && z_up > -1)
    {
      edge_f = z_up - 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_f\n";
    }
    else
      edge_f = -1;

    // edge_g
    if (x_right > -1 && z_up > -1)
    {
      edge_g = z_up + 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_g\n";
    }
    else
      edge_g = -1;

    // edge_h
    if (x_right > -1 && z_down > -1)
    {
      edge_h = z_down + 1;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_h\n";
    }
    else
      edge_h = -1;

    // edge_i
    if (y_left > -1 && z_up > -1)
    {
      edge_i = z_up + px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_i\n";
    }
    else
      edge_i = -1;

    // edge_j
    if (y_left > -1 && x_left > -1)
    {
      edge_j = x_left + px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_j\n";
    }
    else
      edge_j = -1;

    // edge_k
    if (y_left > -1 && z_down > -1)
    {
      edge_k = z_down + px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_k\n";
    }
    else
      edge_k = -1;

    // edge_l
    if (x_right > -1 && y_left > -1)
    {
      edge_l = x_right + px;
      message_count++;
      //SSTMAC_DEBUG << "rank=" << rank << " find edge_l\n";
    }
    else
      edge_l = -1;

    // corner_a
    if (edge_b > -1 && z_up > -1)
    {
      corner_a = edge_b - px * px;
      message_count++;
    }
    else
      corner_a = -1;

    if (edge_d > -1 && z_up > -1)
    {
      corner_b = edge_d - px * px;
      message_count++;
    }
    else
      corner_b = -1;

    if (edge_b > -1 && z_down > -1)
    {
      corner_c = edge_b + px * px;
      message_count++;
    }
    else
      corner_c = -1;

    if (edge_d > -1 && z_down > -1)
    {
      corner_d = edge_d + px * px;
      message_count++;
    }
    else
      corner_d = -1;

    if (edge_j > -1 && z_up > -1)
    {
      corner_e = edge_j - px * px;
      message_count++;
    }
    else
      corner_e = -1;

    if (edge_l > -1 && z_up > -1)
    {
      corner_f = edge_l - px * px;
      message_count++;
    }
    else
      corner_f = -1;

    if (edge_j > -1 && z_down > -1)
    {
      corner_g = edge_j + px * px;
      message_count++;
    }
    else
      corner_g = -1;

    if (edge_l > -1 && z_down > -1)
    {
      corner_h = edge_l + px * px;
      message_count++;
    }
    else
      corner_h = -1;

    //////////////////////////////////////////////////////////////////////////////////////

    int tag(0);
    int x_left_id(x_left % size);
    int x_right_id(x_right % size);
    int y_left_id(y_left % size);
    int y_right_id(y_right % size);
    int z_down_id(z_down % size);
    int z_up_id(z_up % size);
    int edge_a_id(edge_a % size);
    int edge_b_id(edge_b % size);
    int edge_c_id(edge_c % size);
    int edge_d_id(edge_d % size);
    int edge_e_id(edge_e % size);
    int edge_f_id(edge_f % size);
    int edge_g_id(edge_g % size);
    int edge_h_id(edge_h % size);
    int edge_i_id(edge_i % size);
    int edge_j_id(edge_j % size);
    int edge_k_id(edge_k % size);
    int edge_l_id(edge_l % size);
    int corner_a_id(corner_a % size);
    int corner_b_id(corner_b % size);
    int corner_c_id(corner_c % size);
    int corner_d_id(corner_d % size);
    int corner_e_id(corner_e % size);
    int corner_f_id(corner_f % size);
    int corner_g_id(corner_g % size);
    int corner_h_id(corner_h % size);

    const int expected_recvs = message_count;
    //printf("rank=%d, send calculates %d receives were posted.\n", rank, message_count);

    // 3 is the default setting for xfer-fields.
    const int plane_message_size = nx * nx * 3;
    const int pencil_message_size = nx * 3;
    const int corner_message_size = 3;

    if (x_left > -1)
    {
      MPI_Isend(NULL, plane_message_size, MPI_DOUBLE,
          x_left_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted x_left send to " << x_left_id << "\n";
    }

    if (x_right > -1)
    {
      MPI_Isend(NULL, plane_message_size, MPI_DOUBLE,
          x_right_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted x_right send to " << x_right_id << "\n";
    }

    if (y_left > -1)
    {
      MPI_Isend(NULL, plane_message_size, MPI_DOUBLE,
          y_left_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted y_left send to " << y_left_id << "\n";
    }

    if (y_right > -1)
    {
      MPI_Isend(NULL, plane_message_size, MPI_DOUBLE,
          y_right_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted y_right send to " << y_right_id << "\n";
    }

    if (z_down > -1)
    {
      MPI_Isend(NULL, plane_message_size, MPI_DOUBLE,
          z_down_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted z_down send to " << z_down_id << "\n";
    }

    if (z_up > -1)
    {
      MPI_Isend(NULL, plane_message_size, MPI_DOUBLE,
          z_up_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted z_up send to " << z_up_id << "\n";
    }

    if (edge_a > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_a_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_a send to " << edge_a_id << "\n";
    }

    if (edge_b > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_b_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_b send to " << edge_b_id << "\n";
    }

    if (edge_c > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_c_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_c send to " << edge_c_id << "\n";
    }

    if (edge_d > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_d_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_d send to " << edge_d_id << "\n";
    }

    if (edge_e > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_e_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_e send to " << edge_e_id << "\n";
    }

    if (edge_f > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_f_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_f send to " << edge_f_id << "\n";
    }

    if (edge_g > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_g_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_g send to " << edge_g_id << "\n";
    }

    if (edge_h > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_h_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_h send to " << edge_h_id << "\n";
    }

    if (edge_i > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_i_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_i send to "<< edge_i_id << "\n";
    }

    if (edge_j > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_j_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_j send to " << edge_j_id << "\n";
    }

    if (edge_k > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_k_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_k send to " << edge_k_id << "\n";
    }

    if (edge_l > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          edge_l_id, tag, world_, &reqs[message_count++]);
      //SSTMAC_DEBUG << "rank=" << rank << " posted edge_l send to " << edge_l_id << "\n";
    }

    if (corner_a > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_a_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_b > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_b_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_c > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_c_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_d > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_d_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_e > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_e_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_f > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_f_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_g > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_g_id, tag, world_, &reqs[message_count++]);
    }

    if (corner_h > -1)
    {
      MPI_Isend(NULL, pencil_message_size, MPI_DOUBLE,
          corner_h_id, tag, world_, &reqs[message_count++]);
    }

    if ((message_count - expected_recvs) != expected_recvs)
    {
      printf("** RANK=%d / MISMATCH ERROR recv (%d) != sends (%d)\n", rank,
          expected_recvs, (message_count - expected_recvs));
    }

    MPI_Request *req_vec = (MPI_Request*)malloc(message_count * sizeof(MPI_Request));
    MPI_Status *status_vec = (MPI_Status*)malloc(sizeof(MPI_Status) * message_count);

    for (int i = 0; i < message_count; i++)
    {
      req_vec[i] = reqs[i];
    }

    //SSTMAC_DEBUG << "rank=" << rank << " waitall posted: " << message_count << " messages.\n";
    MPI_Waitall(message_count, req_vec, status_vec);
    //SSTMAC_DEBUG << "DONE.\n";

    MPI_Barrier(world_);
  }

  void
  luleshapp::CommMonoQ(int numElem, int numNodes, int px, int nx)
  {
    MPI_Barrier(world_);
  }

  void
  luleshapp::LagrangeLeapFrog(int numElem, int numNodes, int px, int nx)
  {
    LagrangeNodal(numElem, numNodes, px, nx);

    // Comm Recv (IF USING LATE VELOCITY)
    LagrangeElements(numElem, numNodes, px, nx);
    // Send IF USING LATE VELOCITY

    CalcTimeConstraintsForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::LagrangeNodal(int numElem, int numNodes, int px, int nx)
  {
    CalcForceForNodes(numElem, numNodes, px, nx);
    CalcAccelerationForNodes(numElem, numNodes, px, nx);
    ApplyAccelerationBoundaryConditionsForNodes(numElem, numNodes, px, nx);
    CalcVelocityForNodes(numElem, numNodes, px, nx);
    CalcPositionForNodes(numElem, numNodes, px, nx);
  }

  void
  luleshapp::CalcForceForNodes(int numElem, int numNodes, int px, int nx)
  {
    // recv
    comm_recv(numElem, numNodes, px, nx);
    // set all to zero
    CalcVolumeForceForElems(numElem, numNodes, px, nx);
    // Send
    comm_send(numElem, numNodes, px, nx);
    // CommSBN?
  }

  void
  luleshapp::CalcVolumeForceForElems(int numElem, int numNodes, int px, int nx)
  {
    InitStressForElems(numElem, numNodes, px, nx);
    IntegrateStressForElems(numElem, numNodes, px, nx);
    CalcHourglassControlForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::InitStressForElems(int numElem, int numNodes, int px, int nx)
  {

  }

  void
  luleshapp::IntegrateStressForElems(int numElem, int numNodes, int px, int nx)
  {
    /*for(int i = 0; i < numElem; i++) {
     CalcElemShapeFunctionDerivatives();
     CalcElemNodeNormals();
     SumElemStressesToNodeForces();
     }*/

    // no comms in here and everything is done per ELEMENT
    // so we need a per-elem time
    // compute(numElem * 0.0);
    sstmac::timestamp comp_event(numElem * SPTL_INTEGRATE_STRESS_ELEMS);
    app::compute(comp_event);
  }

  void
  luleshapp::CalcHourglassControlForElems(int numElem, int numNodes, int px,
      int nx)
  {
    /* for(int i = 0; i < numElem; i++) {
     CollectDomainNodesToElemNodes();
     CalcElemVolumeDerivative();
     } */

    // no comms in here and everything is done per ELEMENT
    // so do a numElem * wg_hourglass
    // compute(numElem * 0.0);
    sstmac::timestamp comp_event(numElem * SPTL_CALC_HOURGLASS_ELEMS);
    compute(comp_event);

    CalcFBHourglassForceForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::CalcFBHourglassForceForElems(int numElem, int numNodes, int px,
      int nx)
  {
    // SPTL_CALC_FB_HOURGLASS_ELEM
    // wg * numElem
    sstmac::timestamp comp_event(numElem * SPTL_CALC_FB_HOURGLASS_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::CalcAccelerationForNodes(int numElem, int numNodes, int px, int nx)
  {
    /* for(int i = 0; i < numNodes; i++) {
     // calc
     } */

    // no comms in here and everything is done per NODE
    // so do a numNodes * wg
    // compute(numNodes * 0.0);
    sstmac::timestamp comp_event(numNodes * SPTL_CALC_ACCEL_NODES);
    compute(comp_event);
  }

  void
  luleshapp::ApplyAccelerationBoundaryConditionsForNodes(int numElem,
      int numNodes, int px, int nx)
  {
    // seems very very small amount of work - ignore.
  }

  void
  luleshapp::CalcVelocityForNodes(int numElem, int numNodes, int px, int nx)
  {
    /* for(int i = 0; i < numNode; i++) {
     // calc
     }
     */
    // perform a compute numNodes * wg
    // compute(numNodes * 0.0);
    sstmac::timestamp comp_event(numNodes * SPTL_CALC_VEL_NODES);
    compute(comp_event);
  }

  void
  luleshapp::CalcPositionForNodes(int numElem, int numNodes, int px, int nx)
  {
    /* for(int i = 0; i < numNode; i++) {
     // calc
     }
     */
    // perform a compute numNodes * wg
    // compute(numNodes * 0.0);
    sstmac::timestamp comp_event(numNodes * SPTL_CALC_POSITION_NODES);
    compute(comp_event);
  }

  // SPTL_CALC_KINEMATICS_ELEM
  void
  luleshapp::CalcKinematicsForElems(int numElem, int numNodes, int px, int nx)
  {
    // wg * numElem;
    sstmac::timestamp comp_event(numElem * SPTL_CALC_KINEMATICS_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::CalcLagrangeElements(int numElem, int numNodes, int px, int nx)
  {
    CalcKinematicsForElems(numElem, numNodes, px, nx);

    // SPTL_CALC_LAGRANGE_ELEMENTS
    // numElem * wg
    sstmac::timestamp comp_event(numElem * SPTL_CALC_LAGRANGE_ELEMENTS);
    compute(comp_event);
  }

  void
  luleshapp::LagrangeElements(int numElem, int numNodes, int px, int nx)
  {
    CalcLagrangeElements(numElem, numNodes, px, nx);
    CalcQForElems(numElem, numNodes, px, nx);

    ApplyMaterialPropertiesForElems(numElem, numNodes, px, nx);

    UpdateVolumesForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::UpdateVolumesForElems(int numElem, int numNodes, int px, int nx)
  {
    // SPTL_UPDATE_VOL_ELEM
    sstmac::timestamp comp_event(numElem * SPTL_UPDATE_VOL_ELEM);
    compute(comp_event);
  }

  // SPTL_APPLY_MATERIAL_PROP_ELEM
  void
  luleshapp::ApplyMaterialPropertiesForElems(int numElem, int numNodes, int px,
      int nx)
  {
    // numElem * wg.
    sstmac::timestamp comp_event(numElem * SPTL_APPLY_MATERIAL_PROP_ELEM);
    compute(comp_event);

    EvalEOSForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::EvalEOSForElems(int numElem, int numNodes, int px, int nx)
  {
    // SPTL_EVAL_EOS_COMPRESS
    // numElem * wg
    sstmac::timestamp comp_event(numElem * SPTL_EVAL_EOS_COMPRESS);
    compute(comp_event);

    CalcEnergyForElems(numElem, numNodes, px, nx);
    CalcSoundSpeedForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::CalcSoundSpeedForElems(int numElem, int numNodes, int px, int nx)
  {
    // nz * wg;
    // SPTL_CALC_SOUND_SPEED_ELEM
    sstmac::timestamp comp_event(numElem * SPTL_CALC_SOUND_SPEED_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::CalcEnergyForElems(int numElem, int numNodes, int px, int nx)
  {
    sstmac::timestamp comp_event(numElem * SPTL_CALC_ENERGY_FOR_ELEMS);
    compute(comp_event);

    CalcPressureForElems(numElem, numNodes, px, nx);
    CalcPressureForElems(numElem, numNodes, px, nx);
    CalcPressureForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::CalcPressureForElems(int numElem, int numNodes, int px, int nx)
  {
    // SPTL_CALC_PRESSURE_ELEM
    sstmac::timestamp comp_event(numElem * SPTL_CALC_PRESSURE_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::CalcQForElems(int numElem, int numNodes, int px, int nx)
  {
    comm_recv(numElem, numNodes, px, nx);
    CalcMonotonicQGradientsForElems(numElem, numNodes, px, nx);

    comm_send(numElem, numNodes, px, nx);

    // Call to do a barrier.
    CommMonoQ(numElem, numNodes, px, nx);

    CalcMonotonicQForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::CalcMonotonicQForElems(int numElem, int numNodes, int px, int nx)
  {
    CalcMonotonicQRegionForElems(numElem, numNodes, px, nx);
  }

  // SPTL_CALC_MONOTONIC_Q_REGION_ELEM
  void
  luleshapp::CalcMonotonicQRegionForElems(int numElem, int numNodes, int px,
      int nx)
  {
    // numElem * wg.
    sstmac::timestamp comp_event(numElem * SPTL_CALC_MONOTONIC_Q_REGION_ELEM);
    compute(comp_event);
  }

  // SPTL_CALC_MONOTONIC_Q_GRADIENT_ELEM
  void
  luleshapp::CalcMonotonicQGradientsForElems(int numElem, int numNodes, int px,
      int nx)
  {
    sstmac::timestamp comp_event(numElem * SPTL_CALC_MONOTONIC_Q_GRADIENT_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::CalcTimeConstraintsForElems(int numElem, int numNodes, int px,
      int nx)
  {
    CalcCourantConstraintForElems(numElem, numNodes, px, nx);
    CalcHydroConstraintForElems(numElem, numNodes, px, nx);
  }

  void
  luleshapp::CalcCourantConstraintForElems(int numElem, int numNodes, int px,
      int nx)
  {
    // SPTL_CALC_COURANT_ELEM
    sstmac::timestamp comp_event(numElem * SPTL_CALC_COURANT_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::CalcHydroConstraintForElems(int numElem, int numNodes, int px,
      int nx)
  {
    // SPTL_CALC_HYDRO_CONSTRAINT_ELEM
    sstmac::timestamp comp_event(numElem * SPTL_CALC_HYDRO_CONSTRAINT_ELEM);
    compute(comp_event);
  }

  void
  luleshapp::TimeIncrement(int numElem, int numNodes, int px, int nx)
  {
    // all reduce the dt here.
    MPI_Allreduce(NULL, NULL, 1, MPI_DOUBLE,
        MPI_MIN, world_);
  }

  void
  luleshapp::application_main(int iter_max, int numElem, int numNodes, int px,
      int nx)
  {
    int rank, size;
    MPI_Comm_rank(world_, &rank);
    MPI_Comm_size(world_, &size);


    if (rank == 0)
    {
      printf("LULESH Simulation ( %d Ranks )\n", size);
    }

    for (int i = 0; i < iter_max; i++)
    {
      LagrangeLeapFrog(numElem, numNodes, px, nx);
    }
  }

  // Overrides app::consum_params virtual function

  luleshapp::luleshapp(sprockit::sim_parameters* params, software_id sid) :
    app(params, sid)
  {
    usetopo_ = params->get_optional_bool_param("use_libtopomap", false);
    SPTL_INTEGRATE_STRESS_ELEMS = atof(
        params->get_param("SPTL_INTEGRATE_STRESS_ELEMS").c_str());
    SPTL_CALC_HOURGLASS_ELEMS = atof(
        params->get_param("SPTL_CALC_HOURGLASS_ELEMS").c_str());
    SPTL_CALC_ACCEL_NODES = atof(
        params->get_param("SPTL_CALC_ACCEL_NODES").c_str());
    SPTL_CALC_VEL_NODES
        = atof(params->get_param("SPTL_CALC_VEL_NODES").c_str());
    SPTL_CALC_POSITION_NODES = atof(
        params->get_param("SPTL_CALC_POSITION_NODES").c_str());
    SPTL_CALC_KINEMATICS_ELEM = atof(
        params->get_param("SPTL_CALC_KINEMATICS_ELEM").c_str());
    SPTL_CALC_MONOTONIC_Q_GRADIENT_ELEM = atof(
        params->get_param("SPTL_CALC_MONOTONIC_Q_GRADIENT_ELEM").c_str());
    SPTL_CALC_MONOTONIC_Q_REGION_ELEM = atof(
        params->get_param("SPTL_CALC_MONOTONIC_Q_REGION_ELEM").c_str());
    SPTL_APPLY_MATERIAL_PROP_ELEM = atof(
        params->get_param("SPTL_APPLY_MATERIAL_PROP_ELEM").c_str());
    SPTL_EVAL_EOS_COMPRESS = atof(
        params->get_param("SPTL_EVAL_EOS_COMPRESS").c_str());
    SPTL_CALC_PRESSURE_ELEM = atof(
        params->get_param("SPTL_CALC_PRESSURE_ELEM").c_str());
    SPTL_CALC_SOUND_SPEED_ELEM = atof(
        params->get_param("SPTL_CALC_SOUND_SPEED_ELEM").c_str());
    SPTL_UPDATE_VOL_ELEM = atof(
        params->get_param("SPTL_UPDATE_VOL_ELEM").c_str());
    SPTL_CALC_LAGRANGE_ELEMENTS = atof(
        params->get_param("SPTL_CALC_LAGRANGE_ELEMENTS").c_str());
    SPTL_CALC_FB_HOURGLASS_ELEM = atof(
        params->get_param("SPTL_CALC_FB_HOURGLASS_ELEM").c_str());
    SPTL_CALC_COURANT_ELEM = atof(
        params->get_param("SPTL_CALC_COURANT_ELEM").c_str());
    SPTL_CALC_HYDRO_CONSTRAINT_ELEM = atof(
        params->get_param("SPTL_CALC_HYDRO_CONSTRAINT_ELEM").c_str());
    SPTL_CALC_ENERGY_FOR_ELEMS = atof(
        params->get_param("SPTL_CALC_ENERGY_FOR_ELEMS").c_str());

    //printf("SPTL_CALC_ENERGY_FOR_ELEMS = %f\n", SPTL_CALC_ENERGY_FOR_ELEMS);
  }

} // end of namespace hellosim
