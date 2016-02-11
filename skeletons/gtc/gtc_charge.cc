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
   * ----------------------------------------------------------------
   * chargei
   * @param istep
   * ----------------------------------------------------------------
   */
  void
  gtc_main::chargei(int istep)
  {

    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting chargei(" << istep << ") \n";

#ifdef _USE_LOOPS
    libloops_->compute_loop(0, p->mpsi_, 1);

    //$omp parallel do private(m,larmor,psitmp,thetatmp,zetatmp,rhoi,r,ip,jt,ipjt,&
    //$omp& wz1,kk,rdum,ii,wp1,tflr,im,tdum,j00,j01)

    libloops_->compute_loop(1, fa->mi_, 80);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargei_A_mzeta"] = pd->mzeta_;
		param_map_["chargei_A_mgrid"] = fa->mgrid_;
		param_map_["chargei_A_mi"] = fa->mi_;
		param_map_["chargei_A_MPIsize"] = p->numberpe_;
		param_map_["chargei_A_MPIrank"] = p->mype_;
		SSTMAC_compute_eiger(param_map_, "chargei_A.model");
#endif
#endif

    if (istep == 0)
      {
        //SSTMAC_DEBUG << "gtc(" << p->mype_
        //      << "):: chargei() -- this is the first step, exiting.. \n";
        return;
      }

    //$omp do private(m,larmor,weight,kk,wz1,wz0,wp1,wp0,wt10,wt00,wt11,wt01,ij)

#ifdef _USE_LOOPS
    libloops_->compute_loop(1, fa->mi_, 76);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargei_B_mgrid"] = fa->mgrid_;
		param_map_["chargei_B_mi"] = fa->mi_;
		param_map_["chargei_B_MPIrank"] = p->mype_;
		param_map_["chargei_B_MPIsize"] = p->numberpe_;
		param_map_["chargei_B_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "chargei_B.model");
#endif
#endif

    //$omp end parallel

    // If we have a particle decomposition on the toroidal domains, do a reduce
    // operation to add up all the contributions to charge density on the grid

    if (p->npartdom_ > 1)
      {
        //$omp parallel do private(ij,kk)

#ifdef _USE_LOOPS
        libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 2);
#endif

        MPI_Allreduce(NULL, NULL, (fa->mgrid_ * (pd->mzeta_ + 1)), MPI_DOUBLE,
            MPI_SUM, pd->partd_comm_);

      }

#ifdef _USE_LOOPS
    // poloidal end cell, discard ghost cell j=0
    libloops_->compute_loop2(0, p->mpsi_, 0, pd->mzeta_, 1);

    // toroidal end cell
    libloops_->compute_loop(1, fa->mgrid_, 1);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargei_C_mgrid"] = fa->mgrid_;
		param_map_["chargei_C_MPIrank"] = p->mype_;
		param_map_["chargei_C_MPIsize"] = p->numberpe_;
		param_map_["chargei_C_mpsi"] = p->mpsi_;
		param_map_["chargei_C_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "chargei_C.model");
#endif
#endif

    int icount = fa->mgrid_;
    //!!!idest = mod(myrank_toroidal - 1 + ntoroidal, ntoroidal)
    int idest = pd->left_pe_;
    //!!!isource = mod(myrank_toroidal + 1, ntoroidal)
    int isource = pd->right_pe_;
    int isendtag = pd->myrank_toroidal_;
    int irecvtag = isource;

    // send densityi to left and receive from right

    MPI_Status istatus;
    MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
        MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

#ifdef _USE_LOOPS
    if (pd->myrank_toroidal_ == pd->ntoroidal_ - 1)
      {
        // B.C. at zeta=2*pi is shifted
        libloops_->compute_loop(1, fa->mgrid_, 1);
      }
    else
      {
        // B.C. at zeta<2*pi is continuous
        libloops_->compute_loop(1, fa->mgrid_, 1);

      }

    // zero out charge in radial boundary cell
    libloops_->compute_loop(1, fa->mgrid_, 2);

    // flux surface average and normalization

    //$omp parallel do private(i,j,k,ij)
    libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 3);

    // global sum of phi00, broadcast to every toroidal PE

#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargei_D_mgrid"] = fa->mgrid_;
		param_map_["chargei_D_MPIrank"] = p->mype_;
		param_map_["chargei_D_MPIsize"] = p->numberpe_;
		param_map_["chargei_D_mpsi"] = p->mpsi_;
		param_map_["chargei_D_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "chargei_D.model");
#endif
#endif
    MPI_Allreduce(NULL, NULL, p->mpsi_ + 1, MPI_DOUBLE, MPI_SUM,
        pd->toroidal_comm_);

#ifdef _USE_LOOPS
    libloops_->compute_loop(1, p->mpsi_, 1);

    // densityi subtracted (0,0) mode
    //$omp parallel do private(i,j,k,ij)
    libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 3);

    // enforce charge conservation for zonal flow mode

    libloops_->compute_loop(1, p->mpsi_, 3);

//  for diagnostic
    libloops_->compute_loop(1, p->mpsi_, 1);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargei_E_mgrid"] = fa->mgrid_;
		param_map_["chargei_E_MPIrank"] = p->mype_;
		param_map_["chargei_E_MPIsize"] = p->numberpe_;
		param_map_["chargei_E_mpsi"] = p->mpsi_;
		param_map_["chargei_E_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "chargei_E.model");
#endif
#endif

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: done with chargei() \n";

  }

  /**
   * ---------------------------------------------------------------------------------------
   * chargee
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::chargee()
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

#ifdef _USE_LOOPS
    // !$omp parallel do private(m,larmor,psitmp,thetatmp,zetatmp,r,ip,jt,ipjt,wz1,&
    // !$omp& kk,rdum,ii,wp1,tflr,im,tdum,j00,j01)
    libloops_->compute_loop(1, fa->me_, 28);

    // !$omp do private(m,larmor,weight,kk,wz1,wz0,wp1,wp0,wt10,wt00,wt11,wt01,ij)
    libloops_->compute_loop(1, fa->me_, 22);

    // !$omp end parallel

    //  ! If we have a particle decomposition on the toroidal domains, do a reduce
    //  ! operation to add up all the contributions to charge density on the grid

#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargee_A_me"] = fa->me_;
		param_map_["chargee_A_mpsi"] = p->mpsi_;
		param_map_["chargee_A_mgrid"] = fa->mgrid_;
		param_map_["chargee_A_mzeta"] = pd->mzeta_;
		param_map_["chargee_A_MPIsize"] = p->numberpe_;
		param_map_["chargee_A_MPIrank"] = p->mype_;
		SSTMAC_compute_eiger(param_map_, "chargee_A.model");
#endif
#endif
    if (p->npartdom_ > 1)
      {
        // !$omp parallel do private(ij,kk)
#ifdef _USE_LOOPS
        libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 2);
#endif
        MPI_Allreduce(NULL, NULL, (fa->mgrid_ * (pd->mzeta_ + 1)), MPI_DOUBLE,
            MPI_SUM, pd->partd_comm_);

      }

#ifdef _USE_LOOPS
    // ! poloidal end cell
    libloops_->compute_loop(0, p->mpsi_, 1);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargee_B_me"] = fa->me_;
		param_map_["chargee_B_mpsi"] = p->mpsi_;
		param_map_["chargee_B_mgrid"] = fa->mgrid_;
		param_map_["chargee_B_mzeta"] = pd->mzeta_;
		param_map_["chargee_B_MPIrank"] = p->mype_;
		param_map_["chargee_B_MPIsize"] = p->numberpe_;
		SSTMAC_compute_eiger(param_map_, "chargee_B.model");
#endif
#endif

    //  ! toroidal end cell

    int icount = fa->mgrid_;
    //!!idest=mod(mype-1+numberpe,numberpe)
    int idest = pd->left_pe_;
    //!!isource=mod(mype+1,numberpe)
    int isource = pd->right_pe_;
    //!!isendtag=mype
    int isendtag = pd->myrank_toroidal_;
    int irecvtag = isource;
    MPI_Status istatus;

    //! send densitye to left and receive from right
    MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
        MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

#ifdef _USE_LOOPS
    if (pd->myrank_toroidal_ == pd->ntoroidal_ - 1)
      {
        // ! B.C. at zeta=2*pi is shifted
        libloops_->compute_loop(0, p->mpsi_, 3);

      }
    else
      {
        // ! B.C. at zeta<2*pi is continuous

      }

    //  ! zero out charge in radial boundary cell
    libloops_->compute_loop(0, p->nbound_ - 1, 2);

    // ! flux surface average and normalization

    // !$omp parallel do private(i,j,k,ij)
    libloops_->compute_loop2(0, fa->mgrid_, 1, pd->mzeta_, 2);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargee_C_me"] = fa->me_;
		param_map_["chargee_C_mpsi"] = p->mpsi_;
		param_map_["chargee_C_mgrid"] = fa->mgrid_;
		param_map_["chargee_C_mzeta"] = pd->mzeta_;
		param_map_["chargee_C_MPIsize"] = p->numberpe_;
		param_map_["chargee_C_MPIrank"] = p->mype_;
		SSTMAC_compute_eiger(param_map_, "chargee_C.model");
#endif
#endif

    //! toroidal sum of phi00, broadcast to every PE
    MPI_Allreduce(NULL, NULL, p->mpsi_ + 1, MPI_DOUBLE, MPI_SUM,
        pd->toroidal_comm_);

#ifdef _USE_LOOPS
    // ! densitye subtracted (0,0) mode
    // !$omp parallel do private(i,j,k,ij)
    libloops_->compute_loop2(0, fa->mgrid_, 1, pd->mzeta_, 3);

    // ! enforce charge conservation for zonal flow mode
    libloops_->compute_loop(1, p->mpsi_ - 1, 4);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["chargee_D_me"] = fa->me_;
		param_map_["chargee_D_mpsi"] = p->mpsi_;
		param_map_["chargee_D_mgrid"] = fa->mgrid_;
		param_map_["chargee_D_mzeta"] = pd->mzeta_;
		param_map_["chargee_D_MPIsize"] = p->numberpe_;
		param_map_["chargee_D_MPIrank"] = p->mype_;
		SSTMAC_compute_eiger(param_map_, "chargee_D.model");
#endif
#endif

  }
}
