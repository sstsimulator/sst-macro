/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include "gtc_main.h"

namespace gtc
{


  using namespace sstmac;
  using namespace sstmac::sw;

  /**
   * ---------------------------------------------------------------------------------------
   * shifti
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::shifti()
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    int nzion = 2 * p->nparam_;

    int m0 = 1;
    int iteration = 0;

    while (++iteration)
      {
        if(iteration > pd->ntoroidal_)
          {
            MPI_Abort(MPI_COMM_WORLD, 1);
          }

#ifdef _USE_LOOPS
        if (m0 <= fa->mi_)
          {
            // !$omp parallel do private(m)
            libloops_->compute_loop(1, fa->mi_, 1);

            //! NOTE: The vector kzelectron is also used in chargee and pushe to temporarily
            // !    store the closest zeta grid point to the electron "m". S.Ethier 10/7/04

            //  !  This section of code replaces the section above when the compilation does
            //   !  NOT include the OpenMP support option. Temporary arrays msleft and msright
            //   !  are not needed as well as the extra code for thread work distribution.
            libloops_->compute_loop(m0, fa->mi_, 9);

          }
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["shifti_A_mi"] = fa->mi_;
				param_map_["shifti_A_MPIrank"] = p->mype_;
				param_map_["shifti_A_MPIsize"] = p->numberpe_;
				//boost::mt19937 gen;
				//boost::normal_distribution<> ndist(141840,793837);
				//param_map_["shifti_A_n_ops"] = ndist(gen); //this value doesnt exist in model, use average value
				param_map_["shifti_A_n_ops"] = 10576; //this value doesnt exist in model, use average value
				SSTMAC_compute_eiger(param_map_, "shifti_A.model");
#endif
#endif

        if (iteration > 1)
          {
            //! total # of particles to be shifted

            MPI_Allreduce(NULL, NULL, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

            //TODO: Simulate the exit condition correctly
            //! no particle to be shifted, return
            //if( mrevc == 0 )
            if(true)
              {
                break;
              }
          }

        //! an extra space to prevent zero size when msendright(1)=msendleft(1)=0

#ifdef _USE_LOOPS
        //  ! pack particle to move right
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);

        //  ! pack particle to move left
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);

        //  ! # of particles remain on local PE

        //   ! fill the hole
        libloops_->compute_loop(1, 20, 8);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["shifti_B_mi"] = fa->mi_;
				param_map_["shifti_B_MPIrank"] = p->mype_;
				param_map_["shifti_B_MPIsize"] = p->numberpe_;
				param_map_["shifti_B_msend"] = 17607.46; //this number doesnt exist in skeleton, using average
				SSTMAC_compute_eiger(param_map_, "shifti_B.model");
#endif
#endif

        //  ! send # of particle to move right

        int idest = pd->right_pe_;
        int isource = pd->left_pe_;
        int isendtag = pd->myrank_toroidal_;
        int irecvtag = isource;
        MPI_Status istatus;
        MPI_Sendrecv(NULL, 2, MPI_INT, idest, isendtag, NULL, 2, MPI_INT,
            isource, irecvtag, pd->toroidal_comm_, &istatus);

        // ! send particle to right and receive from left

        int isendcount = 5 * nzion;
        int irecvcount = 5 * nzion;

        MPI_Sendrecv(NULL, isendcount, MPI_DOUBLE, idest, isendtag, NULL,
            irecvcount, MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_,
            &istatus);

        // ! send # of particle to move left

        idest = pd->left_pe_;
        isource = pd->right_pe_;
        isendtag = pd->myrank_toroidal_;
        irecvtag = isource;
        MPI_Sendrecv(NULL, 2, MPI_INT, idest, isendtag, NULL, 2, MPI_INT,
            isource, irecvtag, pd->toroidal_comm_, &istatus);

        //  ! send particle to left and receive from right

        isendcount = 5 * nzion;
        irecvcount = 5 * nzion;

        MPI_Sendrecv(NULL, isendcount, MPI_DOUBLE, idest, isendtag, NULL,
            irecvcount, MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_,
            &istatus);

#ifdef _USE_LOOPS
        //  ! unpack particle, particle moved from left
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);

        // ! particle moved from right
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["shifti_C_mi"] = fa->mi_;
				param_map_["shifti_C_MPIrank"] = p->mype_;
				param_map_["shifti_C_MPIsize"] = p->numberpe_;
				SSTMAC_compute_eiger(param_map_, "shifti_C.model");
#endif
#endif

      }

  }

  /**
   * ---------------------------------------------------------------------------------------
   * shifte
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::shifte()
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    int nzelectron = 12;

    int m0 = 1;

    int iteration = 0;

    while (++iteration)
      {

        if(iteration > pd->ntoroidal_)
          {
            MPI_Abort(MPI_COMM_WORLD, 1);
          }

#ifdef _USE_LOOPS
        if (m0 <= fa->me_)
          {
            // !$omp parallel do private(m)
            libloops_->compute_loop(1, fa->me_, 1);

            //! NOTE: The vector kzelectron is also used in chargee and pushe to temporarily
            // !    store the closest zeta grid point to the electron "m". S.Ethier 10/7/04

            //  !  This section of code replaces the section above when the compilation does
            //   !  NOT include the OpenMP support option. Temporary arrays msleft and msright
            //   !  are not needed as well as the extra code for thread work distribution.
            libloops_->compute_loop(m0, fa->me_, 9);

          }
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["shifte_A_me"] = fa->me_;
				param_map_["shifte_A_MPIrank"] = p->mype_;
				param_map_["shifte_A_MPIsize"] = p->numberpe_;
				SSTMAC_compute_eiger(param_map_, "shifte_A.model");
#endif
#endif

        if (iteration > 1)
          {
            //! total # of particles to be shifted

            MPI_Allreduce(NULL, NULL, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

            //TODO: Simulate the exit condition correctly
            //! no particle to be shifted, return
            //if( mrevc == 0 )
            if(true)
              {
                break;
              }
          }

        //! an extra space to prevent zero size when msendright(1)=msendleft(1)=0

#ifdef _USE_LOOPS
        //  ! pack particle to move right
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);

        //  ! pack particle to move left
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);

        //  ! # of particles remain on local PE

        //   ! fill the hole
        libloops_->compute_loop(1, 20, 8);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["shifte_B_me"] = fa->me_;
				param_map_["shifte_B_MPIrank"] = p->mype_;
				param_map_["shifte_B_MPIsize"] = p->numberpe_;
				param_map_["shifte_B_msend"] = 0; //TODO hack, this number doesnt exist in skeleton
				SSTMAC_compute_eiger(param_map_, "shifte_B.model");
#endif
#endif

        //  ! send # of particle to move right

        int idest = pd->right_pe_;
        int isource = pd->left_pe_;
        int isendtag = pd->myrank_toroidal_;
        int irecvtag = isource;
        MPI_Status istatus;
        MPI_Sendrecv(NULL, 2, MPI_INT, idest, isendtag, NULL, 2, MPI_INT,
            isource, irecvtag, pd->toroidal_comm_, &istatus);

        // ! send particle to right and receive from left

        int isendcount = 5 * nzelectron;
        int irecvcount = 5 * nzelectron;

        MPI_Sendrecv(NULL, isendcount, MPI_DOUBLE, idest, isendtag, NULL,
            irecvcount, MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_,
            &istatus);

        // ! send # of particle to move left

        idest = pd->left_pe_;
        isource = pd->right_pe_;
        isendtag = pd->myrank_toroidal_;
        irecvtag = isource;
        MPI_Sendrecv(NULL, 2, MPI_INT, idest, isendtag, NULL, 2, MPI_INT,
            isource, irecvtag, pd->toroidal_comm_, &istatus);

        //  ! send particle to left and receive from right

        isendcount = 5 * nzelectron;
        irecvcount = 5 * nzelectron;

        MPI_Sendrecv(NULL, isendcount, MPI_DOUBLE, idest, isendtag, NULL,
            irecvcount, MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_,
            &istatus);

#ifdef _USE_LOOPS
        //  ! unpack particle, particle moved from left
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);

        // ! particle moved from right
        //  !$omp parallel do private(m)
        libloops_->compute_loop(1, 10, 2);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["shifte_C_me"] = fa->me_;
				param_map_["shifte_C_MPIrank"] = p->mype_;
				param_map_["shifte_C_MPIsize"] = p->numberpe_;
				SSTMAC_compute_eiger(param_map_, "shifte_C.model");
#endif
#endif

      }

  }

}
