
#include <iomanip>
#include <systolicmxm.h>
#include <sstmac/software/libraries/compute/compute_message.h>
#include <math.h>  // ceil

using namespace sstmac;
using namespace sstmac::sw;

namespace mini {

    SpktRegisterApp("systolicmxm", systolicmxm);

    systolicmxm::systolicmxm()
    {
      // these defaults will be ignored -- they are replaced by consume_params
      nrowblocks = 4;
      nlnkblocks = 4;
      ncolblocks = 4;
      blockrowsize = 100;
      blocklnksize = 100;
      blockcolsize = 100;
      nrownodes = 4;
      ncolnodes = 4;
      nrowcorespernode = 1;
      ncolcorespernode = 1;
      systolicsetup = true;
      threads = 1;
    }
                           
    systolicmxm::~systolicmxm() throw ()
    {
    }

    void
    systolicmxm::rank_to_grid(long rank, long &row, long &col)
    {
      long ncorespernode = nrowcorespernode * ncolcorespernode;

      long node = rank / ncorespernode;
      long core = rank % ncorespernode;
      long noderow = node / ncolnodes;
      long nodecol = node % ncolnodes;
      long corerow = core / ncolcorespernode;
      long corecol = core % ncolcorespernode;

      row = noderow * nrowcorespernode + corerow;
      col = nodecol * ncolcorespernode + corecol;
    }

    void
    systolicmxm::grid_to_rank(long row, long col, long &rank)
    {
      long ncorespernode = nrowcorespernode * ncolcorespernode;
      long nrow = nrownodes * nrowcorespernode;
      long ncol = ncolnodes * ncolcorespernode;

      // Handle wrapped boundary conditions
      if (row < 0)
        row += nrow;
      if (row >= nrow)
        row -= nrow;
      if (col < 0)
        col += ncol;
      if (col >= ncol)
        col -= ncol;

      long noderow = row / nrowcorespernode;
      long corerow = row % nrowcorespernode;
      long nodecol = col / ncolcorespernode;
      long corecol = col % ncolcorespernode;
      long node = noderow * ncolnodes + nodecol;
      long core = corerow * ncolcorespernode + corecol;

      rank = node * ncorespernode + core;
    }

    void
    systolicmxm::consume_params(sprockit::sim_parameters* params)
    {
      compute_matrix_ = new lib_compute_matrix(id_);
      register_lib(compute_matrix_);

      long def_nblocks = params->get_optional_long_param("systolicmxm_nblocks", 4);
      long def_blocksize = params->get_optional_long_param("systolicmxm_blocksize", 100);
      long def_nnodes = params->get_optional_long_param("systolicmxm_nnodes", 16);
      long def_ncorespernode = params->get_optional_long_param("systolicmxm_ncorespernode", 1);
      
      long def_nxynodes = 1;
      while ((def_nxynodes+1)*(def_nxynodes+1) <= def_nnodes) {
        def_nxynodes++;
      }

      long def_nxycorespernode = 1;
      while ((def_nxycorespernode+1)*(def_nxycorespernode+1) <= def_ncorespernode) {
        def_nxycorespernode++;
      }
      
      nrowblocks = params->get_optional_long_param("systolicmxm_nrowblocks", def_nblocks);
      nlnkblocks = params->get_optional_long_param("systolicmxm_nlnkblocks", def_nblocks);
      ncolblocks = params->get_optional_long_param("systolicmxm_ncolblocks", def_nblocks);

      blockrowsize = params->get_optional_long_param("systolicmxm_blockrowsize", def_blocksize);
      blocklnksize = params->get_optional_long_param("systolicmxm_blocklnksize", def_blocksize);
      blockcolsize = params->get_optional_long_param("systolicmxm_blockcolsize", def_blocksize);

      nrownodes = params->get_optional_long_param("systolicmxm_nrownodes", def_nxynodes);
      ncolnodes = params->get_optional_long_param("systolicmxm_ncolnodes", def_nxynodes);

      nrowcorespernode = params->get_optional_long_param("systolicmxm_nrowcorespernode", def_nxycorespernode);
      ncolcorespernode = params->get_optional_long_param("systolicmxm_ncolcorespernode", def_nxycorespernode);

     // rowfactor_ = params->get_optional_double_param("systolicmxm_rowfactor", 1.0);
     // colfactor_ = params->get_optional_double_param("systolicmxm_colfactor", 1.0);

      systolicsetup = params->get_optional_bool_param("systolicmxm_systolicsetup", false);

      threads = params->get_optional_long_param("systolicmxm_threads", 1);
    }

    void
    systolicmxm::skeleton_main()
    {
      // input parameters:
      // long nrowblocks
      // long nlnkblocks
      // long ncolblocks
      // long blockrowsize
      // long blocklnksize
      // long blockcolsize
      // long nrownodes
      // long ncolnodes
      // long nrowcorespernode
      // long ncolcorespernode
      
      long blocksize = blockrowsize * blockcolsize;

      // Note 1: there is one MPI task per core
      // Note 2: cores on a node are numbered sequentially

      sstmac::timestamp start = mpi()->init();
      sstmac::sw::mpi_comm* world = mpi()->comm_world();

      long rank = world->rank();
      long size = world->size();

      logger info("<systolicmxm> info", &std::cout);
      logger time("<systolicmxm> time", &std::cout);

      if (rank == 0) {
        if (info.is_active()) info << "systolicmxm: running with the following parameters:\n"
        << "  size of world = " << size << "\n"
        << "  nrowblocks = " << nrowblocks << "\n"
        << "  nlnkblocks = " << nlnkblocks << "\n"
        << "  nrowblocks = " << nrowblocks << "\n"
        << "  blockrowsize = " << blockrowsize << "\n"
        << "  blocklnksize = " << blocklnksize << "\n"
        << "  blockrowsize = " << blockrowsize << "\n"
        << "  nrownodes = " << nrownodes << "\n"
        << "  ncolnodes = " << ncolnodes << "\n"
        << "  nrowcorespernode = " << nrowcorespernode << "\n"
        << "  ncolcorespernode = " << ncolcorespernode << "\n";
      }
     
      // This barrier forces node 0 to print the above information
      // before another node can throw the exception below. It
      // also makes sure all the nodes are ready before timings start.
      //mpi()->barrier(world);
      
      double t0 = mpi()->wtime().sec();

      if (size != nrowcorespernode * ncolcorespernode * nrownodes * ncolnodes)
        {
          throw sprockit::value_error(
              "systolicmxm::run(): there must be one task per core");
        }

      long nrow = nrownodes * nrowcorespernode;
      long ncol = ncolnodes * ncolcorespernode;

     // long nrow = sqrt(size) * rowfactor_;
     // long ncol = sqrt(size) * colfactor_;
      nrowcorespernode = 1;
      ncolcorespernode = 1;
      nrownodes = nrow;
      ncolnodes = ncol;

      if (nrow > nrowblocks || nrow > nlnkblocks || ncol > ncolblocks
          || ncol > nlnkblocks)
        {
          throw sprockit::value_error(
              "systolicmxm::run(): too many nodes to wrap matrices onto all nodes");
        }

      // Compute my row and col
      long myrow, mycol;
      rank_to_grid(rank, myrow, mycol);

      // Compute my neighbors' rows and columns
      long myup, mydn, mylf, myrt;
      grid_to_rank(myrow - 1, mycol, myup);
      grid_to_rank(myrow + 1, mycol, mydn);
      grid_to_rank(myrow, mycol + 1, myrt);
      grid_to_rank(myrow, mycol - 1, mylf);

      // Compute the number of row, alnk, blnk, and col blocks on the current node.
      // alnk blocks are col blocks of the A matrix
      // blnk blocks are row blocks of the B matrix
      //long nlocalrowblocks = nrowblocks/nrow + ((myrow < nrowblocks%nrow)? 1:0);
      //long nlocalcolblocks = ncolblocks/ncol + ((mycol < ncolblocks%ncol)? 1:0);
      long nlocalalnkblocks = nlnkblocks / ncol
          + ((mycol < nlnkblocks % ncol) ? 1 : 0);
      long nlocalblnkblocks = nlnkblocks / nrow
          + ((myrow < nlnkblocks % nrow) ? 1 : 0);

      ////////////////////////////////////////////////////////////
      // Move A and B into their initial positions

      // Example 3x3 starting position:
      //  A00 | A01 | A02
      //  A10 | A11 | A12
      //  A20 | A21 | A22
      // Layout of A and B to begin mxm:
      //  A00 B00 | A01 B11 | A02 B22
      //  A11 B10 | A12 B21 | A02 B10
      //  A22 B20 | A20 B01 | A21 B12
      //  - rows of A are shifted myrow cols left
      //  - cols of B are shifted mycol rows up
      if (systolicsetup)
        {
          if (nrowblocks != ncolblocks || nrowblocks != nlnkblocks
              || nrowblocks != nrow || nrowblocks != ncol)
            {
              throw sprockit::unimplemented_error(
                  "systolicmxm::systolic setup: must have one block per rank");
            }

          // cyclically shift the A and B blocks to their starting point
          for (int i = 0; i < myrow || i < mycol; i++)
            {
              std::vector<mpi_request*> reqs;
              if (i < myrow)
                {
                  mpi_request* req;
                  mpi()->isend(blockrowsize * blocklnksize, mpi_type::mpi_double->id,
                      mpi_id(mylf), mpi_tag(0), world, req);
                  reqs.push_back(req);
                  mpi()->irecv(blockrowsize * blocklnksize, mpi_type::mpi_double->id,
                      mpi_id(myrt), mpi_tag(0), world, req);
                  reqs.push_back(req);
                }
              if (i < mycol)
                {
                  mpi_request* req;
                  mpi()->isend(blockcolsize * blocklnksize, mpi_type::mpi_double->id,
                      mpi_id(myup), mpi_tag(0), world, req);
                  reqs.push_back(req);
                  mpi()->irecv(blockcolsize * blocklnksize, mpi_type::mpi_double->id,
                      mpi_id(mydn), mpi_tag(0), world, req);
                  reqs.push_back(req);
                }
              std::vector<mpi_status> statuses;
              mpi()->waitall(reqs, statuses);
            }
        }
      else
        {
          // directly send the A and B blocks to their starting point
          std::vector<mpi_request*> reqs;
          long targetrank;
          if (myrow > 0)
            {
              mpi_request* req;
              grid_to_rank(myrow, mycol - myrow, targetrank);
              mpi()->isend(blockrowsize * blocklnksize, mpi_type::mpi_double->id,
                  mpi_id(targetrank), mpi_tag(0), world, req);
              reqs.push_back(req);
              grid_to_rank(myrow, mycol + myrow, targetrank);
              mpi()->irecv(blockrowsize * blocklnksize, mpi_type::mpi_double->id,
                  mpi_id(targetrank), mpi_tag(0), world, req);
              reqs.push_back(req);
            }
          if (mycol > 0)
            {
              mpi_request* req;
              grid_to_rank(myrow - mycol, mycol, targetrank);
              mpi()->isend(blockcolsize * blocklnksize, mpi_type::mpi_double->id,
                  mpi_id(targetrank), mpi_tag(1), world, req);
              reqs.push_back(req);
              grid_to_rank(myrow + mycol, mycol, targetrank);
              mpi()->irecv(blockcolsize * blocklnksize, mpi_type::mpi_double->id,
                  mpi_id(targetrank), mpi_tag(1), world, req);
              reqs.push_back(req);
            }
          std::vector<mpi_status> statuses;
          mpi()->waitall(reqs, statuses);
        }

      ////////////////////////////////////////////////////////////
      // Compute the mxm

      // iterate through the matrix multiply in nblock-1 steps
      //  - A is shifted up
      //  - B is shifted left
      for (int i = 0; i < nlnkblocks; i++)
        {
          std::vector<mpi_request*> reqs;
          // Only begin to send data when we have run out of local blocks
          // to work on. This will cause an implicit synchronization
          // with nodes that have fewer blocks.
          if (i < nlnkblocks - nlocalalnkblocks)
            {
              mpi_request* req;
              mpi()->isend(blocksize, mpi_type::mpi_double->id, mpi_id(mylf),
                  mpi_tag(0), world, req);
              reqs.push_back(req);
              mpi()->irecv(blocksize, mpi_type::mpi_double->id, mpi_id(myrt),
                  mpi_tag(0), world, req);
              reqs.push_back(req);
            }
          // Only begin to send data when we have run out of local blocks
          // to work on. This will cause an implicit synchronization
          // with nodes that have fewer blocks.
          if (i < nlnkblocks - nlocalblnkblocks)
            {
              mpi_request* req;
              mpi()->isend(blocksize, mpi_type::mpi_double->id, mpi_id(myup),
                  mpi_tag(0), world, req);
              reqs.push_back(req);
              mpi()->irecv(blocksize, mpi_type::mpi_double->id, mpi_id(mydn),
                  mpi_tag(0), world, req);
              reqs.push_back(req);
            }
          // We have at least one new A and B block with matching
          // link block numbers, so do the computation.
          compute_matrix_->double_mxm(blockrowsize, blockcolsize, blocklnksize,
              threads);
          std::vector<mpi_status> statuses;
          mpi()->waitall(reqs, statuses);
        }

      compute_matrix_->double_mxm(blockrowsize, blockcolsize, blocklnksize,
                   threads);

      //gather the result
      mpi()->gather(blockrowsize * blockcolsize, mpi_type::mpi_double->id, 
        size  * blockrowsize * blockcolsize, mpi_type::mpi_double->id, mpi_id(0), world);

     /* mpi()->barrier(world);
      if (rank == 0) {
        double t1 = mpi()->wtime().sec();
        if (time.is_active()) time << "total time = " << t1-t0 << "\n";
      }*/

      mpi()->finalize();
    }

} //end namespace
