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
   * pushi
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::pushi(int istep)
  {
    //short names

    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting pushi(" << istep << ") \n";

    p->limit_vpara_ = 0; // limit_vpara=1 :  parallel velocity kept <= abs(umax)
    int conserve_particles = 0;

#ifdef _USE_LOOPS
    if (p->irk_ == 1)
      {
        // 1st step of Runge-Kutta method

        //$omp parallel do private(m)
        libloops_->compute_loop2(1, fa->mi_, 1, 5, 1);


        // 2nd step of Runge-Kutta method
      }
    else
      {

        if (p->nonlinear_ > 0.5)
          {
            libloops_->compute_loop(1, p->mflux_, 1);
          }
      }

    // gather e_field using 4-point gyro-averaging, sorting in poloidal angle
    // The following line is an OpenMP directive for loop-level parallelism
    // on shared memory machines (see http://www.openmp.org).
    //$omp parallel do private(m,e1,e2,e3,kk,wz1,wz0,larmor,ij,wp0,wt00,&
    //$omp& wt10,wp1,wt01,wt11)
    libloops_->compute_loop2(1, fa->mi_, 1, 4, 1);


    // primary ion marker temperature and parallel flow velocity
    libloops_->compute_loop(1, p->mpsi_, 1);


    //!********** test gradual kappan **********
    //!     kappati=min(6.9_wp,(kappan+(6.9_wp-kappan)*real(istep,wp)/4000._wp))
    //!     if(mype==0.and.irk==2)write(36,*)istep,kappan,kappati
    //! ****************************************

    //! update GC position
    //! Another OpenMP parallel loop...
    //!$omp parallel do private(m,r,rinv,ii,ip,wp0,wp1,tem,q,qinv,cost,sint,cost0,&
    //!$omp& sint0,b,g,gp,ri,rip,dbdp,dbdt,dedb,deni,upara,energy,rfac,kappa,dptdp,&
    //!$omp& dptdt,dptdz,epara,vdr,wdrive,wpara,wdrift,wdot,pdot,tdot,zdot,rdot)
    libloops_->compute_loop(1, fa->mi_, 60);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["pushi_A_mi"] = fa->mi_;
		param_map_["pushi_A_MPIrank"] = p->mype_;
		param_map_["pushi_A_MPIsize"] = p->numberpe_;
		param_map_["pushi_A_irk"] = p->irk_;
		SSTMAC_compute_eiger(param_map_, "pushi_A.model");
#endif
#endif

    if (p->irk_ == 2)
      {

#ifdef _USE_LOOPS
        //! Avoid runaway particles by limiting absolute value of parallel velocity
        //! if parameter limit_vpara=1
        if (p->limit_vpara_ == 1)
          {

            libloops_->compute_loop(1, fa->mi_, 5);

          }

        //! out of boundary particle
        //!$omp parallel do private(m)
        libloops_->compute_loop(1, fa->mi_, 12);
#endif

        double sum_of_weights = 0.0;
        double total_sum_of_weights = 0.0;
        int mi_total;
        double residual_weight;
        if (conserve_particles == 1)
          {
#ifdef _USE_LOOPS
            libloops_->compute_loop(1, fa->mi_, 1);
#endif

            MPI_Allreduce(NULL, NULL, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

            MPI_Allreduce(NULL, NULL, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

#ifdef _USE_LOOPS
            libloops_->compute_loop(1, fa->mi_, 1);
#endif

          }
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["pushi_B_mi"] = fa->mi_;
				param_map_["pushi_B_MPIrank"] = p->mype_;
				param_map_["pushi_B_MPIsize"] = p->numberpe_;
				SSTMAC_compute_eiger(param_map_, "pushi_B.model");
#endif

        //! Restore temperature profile when running a nonlinear calculation
        //! (nonlinear=1.0) and parameter fixed_Tprofile > 0.

        if (p->nonlinear_ > 0.5 && p->fixed_Tprofile > 0)
          {
            if ((istep % p->ndiag_) == 0)
              {

#ifdef _USE_LOOPS
                //!$omp parallel do private(m,cost,b,upara)
                libloops_->compute_loop(1, fa->mi_, 5);

                libloops_->compute_loop(1, fa->mi_, 3);
#else
#ifdef _USE_EIGER
								param_map_.clear();
								param_map_["pushi_C_mi"] = fa->mi_;
								param_map_["pushi_C_MPIsize"] = p->numberpe_;
								param_map_["pushi_C_MPIrank"] = p->mype_;
								SSTMAC_compute_eiger(param_map_, "pushi_C.model");
#endif
#endif

                // ! S.Ethier 06/04/03  According to the MPI standard, the send and receive
                // ! buffers cannot be the same for MPI_Reduce or MPI_Allreduce. It generates
                // ! an error on the Linux platform. We thus make sure that the 2 buffers
                // ! are different.

                MPI_Allreduce(NULL, NULL, p->mflux_, MPI_DOUBLE, MPI_SUM,
                    MPI_COMM_WORLD);

                MPI_Allreduce(NULL, NULL, p->mflux_, MPI_DOUBLE, MPI_SUM,
                    MPI_COMM_WORLD);

#ifdef _USE_LOOPS
                libloops_->compute_loop(1, p->mflux_, 3);

                // !$omp parallel do private(m,ip)
                libloops_->compute_loop(1, fa->mi_, 2);
#else
#ifdef _USE_EIGER
								param_map_.clear();
								param_map_["pushi_D_mi"] = fa->mi_;
								param_map_["pushi_D_MPIsize"] = p->numberpe_;
								param_map_["pushi_D_MPIrank"] = p->mype_;
								SSTMAC_compute_eiger(param_map_, "pushi_D.model");
#endif
#endif

              }
          }
      }

    if (p->idiag_ == 0)
      {
        //! fluxes diagnose at irk=1

#ifdef _USE_LOOPS
        libloops_->compute_loop(1, fa->mi_, 15);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["pushi_E_mi"] = fa->mi_;
				param_map_["pushi_E_MPIsize"] = p->numberpe_;
				param_map_["pushi_E_MPIrank"] = p->mype_;
				SSTMAC_compute_eiger(param_map_, "pushi_E.model");
#endif
#endif

        MPI_Allreduce(NULL, NULL, p->mpsi_ + 1, MPI_DOUBLE, MPI_SUM,
            MPI_COMM_WORLD);

#ifdef _USE_LOOPS
        libloops_->compute_loop(1, p->mpsi_, 1);
#endif

        MPI_Allreduce(NULL, NULL, p->mflux_, MPI_DOUBLE, MPI_SUM,
            MPI_COMM_WORLD);

        MPI_Allreduce(NULL, NULL, p->mflux_, MPI_DOUBLE, MPI_SUM,
            MPI_COMM_WORLD);

#ifdef _USE_LOOPS
        libloops_->compute_loop(1, p->mpsi_, 3);

        //! Field energy: 1/(4*pi)sum_ijk{|Grad_phi|^2*Jacobian*r*deltar*deltat*deltaz}
        libloops_->compute_loop2(1, pd->mzeta_, 1, p->mpsi_, 17);

        //  ! Sum up the values from all the toroidal domains. We need the value of
        //  ! only one processor per domain.

#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["pushi_F_mzeta"] = pd->mzeta_;
				param_map_["pushi_F_mpsi"] = p->mpsi_;
				param_map_["pushi_F_mthetamax"] = p->mthetamax_;
				param_map_["pushi_F_MPIsize"] = p->numberpe_;
				param_map_["pushi_F_MPIrank"] = p->mype_;
				SSTMAC_compute_eiger(param_map_, "pushi_F.model");
#endif
#endif
        MPI_Reduce(NULL, NULL, 3, MPI_DOUBLE, MPI_SUM, 0, pd->toroidal_comm_);

      }

  }

  /**
   * ---------------------------------------------------------------------------------------
   * pushe
   * @param icycle
   * @param irke
   * @param istep
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::pushe(int icycle, int irke, int istep)
  {
    //short names

    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting pushe(" << icycle << ", " << irke << ", " << istep << ") \n";

#ifdef _USE_LOOPS
    if (irke == 1)
      {
        // 1st step of Runge-Kutta method

        if (icycle == 1)
          {
            // save electron info
            if (p->irk_ * p->ihybrid_ == 1)
              {
                p->ntracer1_ = p->ntracer_;
                fa->me1_ = fa->me_;
                //$omp parallel do private(m)
                libloops_->compute_loop2(1, fa->me_, 1, 6, 1);

              }
            else
              {
                // electron start from initial position
                p->ntracer_ = p->ntracer1_;
                fa->me_ = fa->me1_;
                //$omp parallel do private(m)
                libloops_->compute_loop2(1, fa->me_, 1, 6, 1);

              }
          }

        //$omp parallel do private(m)
        libloops_->compute_loop2(1, fa->me_, 1, 5, 1);

        // 2nd step of Runge-Kutta method
      }
    else
      {

      }

    //$omp parallel do private(m,psitmp,thetatmp,zetatmp,r,ip,jt,ipjt,wz1,&
    //$omp& kk,rdum,ii,wp1,tflr,im,tdum,j00,j01)
    libloops_->compute_loop(1, fa->me_, 28);

    //  ! gather e_field using 4-point gyro-averaging, sorting in poloidal angle
    //  ! The following line is an OpenMP directive for loop-level parallelism
    //  ! on shared memory machines (see http://www.openmp.org).
    //  !$omp parallel do private(m,e1,e2,e3,e4,kk,wz1,wz0,ij,wp0,wt00,&
    //  !$omp& wt10,wp1,wt01,wt11)
    libloops_->compute_loop(1, fa->me_, 38);

    //! marker temperature
    libloops_->compute_loop(1, p->mpsi_, 1);

    // ! update GC position
    // ! Another OpenMP parallel loop...
    // !$omp parallel do private(m,r,rinv,ii,wp0,wp1,tem,dzonal,q,qinv,&
    // !$omp& cost,sint,cost0,sint0,&
    // !$omp& b,g,gp,ri,rip,dbdp,dbdt,dedb,deni,upara,energy,rfac,kappa,dptdp,&
    // !$omp& dptdt,dptdz,pptpt,wpara,wdrift,vdr,wdrive,wdot,pdot,tdot,zdot,rdot)
    libloops_->compute_loop(1, fa->me_, 54);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["pushe_A_me"] = fa->me_;
		param_map_["pushe_A_MPIrank"] = p->mype_;
		param_map_["pushe_A_MPIsize"] = p->numberpe_;
		SSTMAC_compute_eiger(param_map_, "pushe_A.model");
#endif
#endif

    if (irke == 2)
      {
        //  ! out of boundary particle
        //  !$omp parallel do private(m)
#ifdef _USE_LOOPS
        libloops_->compute_loop(1, fa->me_, 12);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["pushe_B_me"] = fa->me_;
				param_map_["pushe_B_MPIrank"] = p->mype_;
				param_map_["pushe_B_MPIsize"] = p->numberpe_;
				SSTMAC_compute_eiger(param_map_, "pushe_B.model");
#endif
#endif

        // ! restore temperature profile

        if (p->nonlinear_ > 0.5)
          {
            if ((istep % p->ndiag_) == 0 && p->ihybrid_ == p->nhybrid_
                && icycle == 2 * p->ncycle_)
              {
#ifdef _USE_LOOPS
                libloops_->compute_loop(1, fa->me_, 4);
#else
#ifdef _USE_EIGER
								param_map_.clear();
								param_map_["pushe_C_me"] = fa->me_;
								param_map_["pushe_C_MPIrank"] = p->mype_;
								param_map_["pushe_C_MPIsize"] = p->numberpe_;
								SSTMAC_compute_eiger(param_map_, "pushe_C.model");
#endif
#endif

                MPI_Allreduce(NULL, NULL, p->mflux_, MPI_DOUBLE, MPI_SUM,
                    MPI_COMM_WORLD);

                MPI_Allreduce(NULL, NULL, p->mflux_, MPI_DOUBLE, MPI_SUM,
                    MPI_COMM_WORLD);

#ifdef _USE_LOOPS
                // !          dtem=dtem*tem_inv/max(1.0d+00,dden) !perturbed thermal energy
                libloops_->compute_loop(1, p->mflux_, 3);

                // !$omp parallel do private(m,r,ip)
                libloops_->compute_loop(1, fa->me_, 3);
#else

#ifdef SSTMAC
    param_map_.clear();
    param_map_["pushe_D_me"] = fa->me_;
    param_map_["pushe_D_MPIsize"] = p->numberpe_;
    param_map_["pushe_D_MPIrank"] = p->mype_;
    SSTMAC_compute_eiger(param_map_, "pushe_D.model");
#endif


#endif

              }
          }

      }

    if (p->idiag_ == 0 && p->ihybrid_ * icycle * irke == 1)
      {
        //! fluxes diagnose

#ifdef _USE_LOOPS
        libloops_->compute_loop(1, fa->me_, 5);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["pushe_E_me"] = fa->me_;
				param_map_["pushe_E_MPIsize"] = p->numberpe_;
				param_map_["pushe_E_MPIrank"] = p->mype_;
				SSTMAC_compute_eiger(param_map_, "pushe_E.model");
#endif
#endif

      }

  }

}