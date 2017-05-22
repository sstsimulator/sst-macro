/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
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