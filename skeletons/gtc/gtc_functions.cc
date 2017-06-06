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

#include <gtc_main.h>

namespace gtc
{

  using namespace sstmac;
  using namespace sstmac::sw;

  /**
   * ---------------------------------------------------------------------------------------
   * smooth
   * @param something
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::smooth(int iflag)
  {

    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting smooth(" << iflag << ") \n";

    double scale = 1.0;

#ifdef _USE_LOOPS
		//$omp parallel do private(i)
		libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 1);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["smooth_A_mgrid"] = fa->mgrid_;
		param_map_["smooth_A_MPIrank"] = p->mype_;
		param_map_["smooth_A_MPIsize"] = p->numberpe_;
		param_map_["smooth_A_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "smooth_A.model");
#endif
#endif

    int ismooth = 1;
    if (p->nonlinear_ < 0.5)
      {
        ismooth = 0;
      }

    for (int ip = 1; ip <= ismooth; ip++)
      {
        //radial smoothing

#ifdef _USE_LOOPS
        libloops_->compute_loop2(1, p->mpsi_, 1, pd->mzeta_, 1);

        libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_ + 1, 4);

        // poloidal smoothing (-0.0625 0.25 0.625 0.25 -0.0625)
        //$omp parallel do private(i,j,ii,jt,pright)
        libloops_->compute_loop(1, fa->mgrid_, 2);

        // parallel smoothing
        // send phi to right and receive from left
        libloops_->compute_loop(1, fa->mgrid_, 1);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["smooth_B_mgrid"] = fa->mgrid_;
				param_map_["smooth_B_MPIrank"] = p->mype_;
				param_map_["smooth_B_MPIsize"] = p->numberpe_;
				param_map_["smooth_B_mpsi"] = p->mpsi_;
				param_map_["smooth_B_mzeta"] = pd->mzeta_;
				param_map_["smooth_B_mthetamax"] = p->mthetamax_;
				SSTMAC_compute_eiger(param_map_, "smooth_B.model");
#endif
#endif

        int icount = fa->mgrid_;
        int idest = pd->right_pe_;
        int isource = pd->left_pe_;
        int isendtag = pd->myrank_toroidal_;
        int irecvtag = isource;
        MPI_Status istatus;

        MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
            MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

#ifdef _USE_LOOPS
        // send phi to left and receive from right
        libloops_->compute_loop(1, fa->mgrid_, 1);
#endif

        idest = pd->left_pe_;
        isource = pd->right_pe_;
        isendtag = pd->myrank_toroidal_;
        irecvtag = isource;

        MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
            MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

#ifdef _USE_LOOPS
        //$omp parallel do private(i,ii,jt,j,ij,ptemp,pleft,pright)
        libloops_->compute_loop(1, fa->mgrid_, 3);
        libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 3);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["smooth_C_mgrid"] = fa->mgrid_;
				param_map_["smooth_C_MPIrank"] = p->mype_;
				param_map_["smooth_C_MPIsize"] = p->numberpe_;
				param_map_["smooth_C_mpsi"] = p->mpsi_;
				param_map_["smooth_C_mthetamax"] = p->mthetamax_;
				param_map_["smooth_C_mzeta"] = pd->mzeta_;
				SSTMAC_compute_eiger(param_map_, "smooth_C.model");
#endif
#endif

      }

    // toroidal BC: send phi to right and receive from left
#ifdef _USE_LOOPS
    libloops_->compute_loop(1, fa->mgrid_, 1);
#endif

    int icount = fa->mgrid_;
    int idest = pd->right_pe_;
    int isource = pd->left_pe_;
    int isendtag = pd->myrank_toroidal_;
    int irecvtag = isource;
    MPI_Status istatus;

    MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
        MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

#ifdef _USE_LOOPS
		libloops_->compute_loop(1, fa->mgrid_, 1);

		// poloidal BC
    libloops_->compute_loop2(1, p->mpsi_, 1, pd->mzeta_, 1);

    // radial boundary
    libloops_->compute_loop2(1, p->mpsi_, 1, pd->mzeta_, 1);

    if (iflag == 0)
      {
        //$omp parallel do private(i)
        libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 1);

      }
    else if (iflag == 1)
      {
        //$omp parallel do private(i)
        libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 1);

      }
    else
      {
        //$omp parallel do private(i)
        libloops_->compute_loop2(1, fa->mgrid_, 1, pd->mzeta_, 1);

      }

    // solve zonal flow: phi00=r*E_r, E_r(a0)=0. Trapezoid rule
    if (iflag == 3)
      {
        if (p->nhybrid_ == 0)
          {
            libloops_->compute_loop(1, p->mpsi_, 1);

          }
        if (p->nhybrid_ > 0)
          {
            libloops_->compute_loop(1, p->mpsi_, 1);

          }
        // (-0.0625 0.25 0.625 0.25 -0.0625) radial smoothing of (0,0) mode density phip00

        libloops_->compute_loop(1, p->mpsi_, 1);

        libloops_->compute_loop(1, p->mpsi_, 2);

        libloops_->compute_loop(1, p->mpsi_, 2);

        // subtract net momentum
        // phip00=phip00-sum(phip00)/real(mpsi+1)

        // d phi/dr, in equilibrium unit
        libloops_->compute_loop(1, p->mpsi_, 2);

        // add FLR contribution using Pade approximation: b*<phi>=(1+b)*<n>
        libloops_->compute_loop(1, p->mpsi_, 1);

        libloops_->compute_loop(1, p->mpsi_, 1);

        // (0,0) mode potential store in phi00
        libloops_->compute_loop(1, p->mpsi_, 1);

      }

#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["smooth_D_mgrid"] = fa->mgrid_;
		param_map_["smooth_D_MPIrank"] = p->mype_;
		param_map_["smooth_D_MPIsize"] = p->numberpe_;
		param_map_["smooth_D_mpsi"] = p->mpsi_;
		param_map_["smooth_D_mthetamax"] = p->mthetamax_;
		param_map_["smooth_D_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "smooth_D.model");
#endif
#endif

    // Interpolate on a flux surface from fieldline coordinates to magnetic
    // coordinates. Use mtdiag for both poloidal and toroidal grid points.
    if (iflag > 1)
      {
        if (p->nonlinear_ < 0.5 || (iflag == 3 && p->idiag_ == 0))
          {

            int mzbig = std::max(1, p->mtdiag_ / p->mzetamax_);
            int mzmax = p->mzetamax_ * mzbig;
            int mz = pd->mzeta_ * mzbig;
            int meachtheta = p->mtdiag_ / pd->ntoroidal_;
            int icount = meachtheta * mz * (p->idiag2_ - p->idiag1_ + 1);

#ifdef _USE_LOOPS
            libloops_->compute_loop4(1, pd->mzeta_, 1, mzbig, p->idiag1_,
                p->idiag2_, 1, p->mtdiag_, 6);
#else
#ifdef _USE_EIGER
						param_map_.clear();
						param_map_["smooth_E_mgrid"] = fa->mgrid_;
						param_map_["smooth_E_MPIrank"] = p->mype_;
						param_map_["smooth_E_MPIsize"] = p->numberpe_;
						param_map_["smooth_E_mpsi"] = p->mpsi_;
						param_map_["smooth_E_mthetamax"] = p->mthetamax_;
						param_map_["smooth_E_mzeta"] = pd->mzeta_;
						param_map_["smooth_E_mtdiag"] = p->mtdiag_;
						SSTMAC_compute_eiger(param_map_, "smooth_E.model");
#endif
#endif

            // transpose 2-d matrix from (ntoroidal,mzeta*mzbig) to (1,mzetamax*mzbig)
            for (int jpe = 0; jpe <= pd->ntoroidal_ - 1; jpe++)
              {

#ifdef _USE_LOOPS
                // $omp parallel do private(j,i,k,jt,indt,indp1,indp)
                libloops_->compute_loop3(1, meachtheta, p->idiag1_, p->idiag2_, 1,
                    mz, 2);
#endif

                MPI_Gather(NULL, icount, MPI_DOUBLE, NULL, icount, MPI_DOUBLE,
                    jpe, pd->toroidal_comm_);

              }

#ifdef _USE_LOOPS
            // transform to k space
            //$omp parallel do private(j,i,kz,k,indt1,indt,indp,xz,yz) firstprivate(aux2f,aux3f,aux2b,aux3b)
            for (int j = 1; j <= meachtheta; j++)
              {
                int indt1 = (j - 1) * mz;
                for (int i = p->idiag1_; i <= p->idiag2_; i++)
                  {

                    libloops_->compute_loop2(1, pd->ntoroidal_, 1, mz, 2);

                    libloops_->compute_fft();

                    if (p->nonlinear_ < 0.5)
                      {
                        // linear run only keep a few modes
                        libloops_->compute_loop(1, p->mtdiag_, 1);

                        libloops_->compute_fft();

                        // transpose back to (ntoroidal,mz)
                        libloops_->compute_loop2(1, pd->ntoroidal_, 1, mz, 2);

                      }
                  }
              }
#endif

            if (p->nonlinear_ < 0.5)
              {
                for (int jpe = 0; jpe <= pd->ntoroidal_ - 1; jpe++)
                  {

                    MPI_Scatter(NULL, icount, MPI_DOUBLE, NULL, icount,
                        MPI_DOUBLE, jpe, pd->toroidal_comm_);

#ifdef _USE_LOOPS
                    //$omp parallel do private(j,i,k,jt,indt,indp1,indp)
                    libloops_->compute_loop3(1, meachtheta, p->idiag1_, p->idiag2_,
                        1, mz, 2);
#endif
                  }

                // interpolate field from magnetic coordinates to fieldline coordinates
#ifdef _USE_LOOPS
                libloops_->compute_loop3(1, pd->mzeta_, p->idiag1_, p->idiag2_, 1,
                    p->mthetamax_, 7);

                // toroidal BC: send phi to right and receive from left
                libloops_->compute_loop(1, fa->mgrid_, 1);
#else
#ifdef _USE_EIGER
								param_map_.clear();
								param_map_["smooth_G_mpsi"] = p->mpsi_;
								param_map_["smooth_G_mgrid"] = fa->mgrid_;
								param_map_["smooth_G_mzeta"] = pd->mzeta_;
								param_map_["smooth_G_mthetamax"] = p->mthetamax_;
								param_map_["smooth_G_mtdiag"] = p->mtdiag_;
								param_map_["smooth_G_MPIrank"] = p->mype_;
								param_map_["smooth_G_MPIsize"] = p->numberpe_;
								param_map_["smooth_G_meachtheta"] = meachtheta;
								SSTMAC_compute_eiger(param_map_, "smooth_G.model");
#endif
#endif

                icount = fa->mgrid_;
                idest = pd->right_pe_;
                isource = pd->left_pe_;
                isendtag = pd->myrank_toroidal_;
                irecvtag = isource;

                MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL,
                    icount, MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_,
                    &istatus);

#ifdef _USE_LOOPS
                if (pd->myrank_toroidal_ == 0)
                  {
                    libloops_->compute_loop2(p->idiag1_, p->idiag2_, 1,
                        p->mthetamax_, 1);

                  }
                else
                  {
                    libloops_->compute_loop(1, fa->mgrid_, 1);

                  }

                // poloidal BC
                libloops_->compute_loop2(p->idiag1_, p->idiag2_, 1, pd->mzeta_, 1);
#else
#ifdef _USE_EIGER
								param_map_.clear();
								param_map_["smooth_H_mpsi"] = p->mpsi_;
								param_map_["smooth_H_mgrid"] = fa->mgrid_;
								param_map_["smooth_H_mzeta"] = pd->mzeta_;
								param_map_["smooth_H_mthetamax"] = p->mthetamax_;
								param_map_["smooth_H_mtdiag"] = p->mtdiag_;
								param_map_["smooth_H_MPIrank"] = p->mype_;
								param_map_["smooth_H_MPIsize"] = p->numberpe_;
								SSTMAC_compute_eiger(param_map_, "smooth_H.model");
#endif
#endif
              }
          }
      }

    if (iflag == 2)
      {
        int j = p->irk_ + 2 * (p->ihybrid_ - 1);

#ifdef _USE_LOOPS
        // $omp parallel do private(i)
        libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 1);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["smooth_J_mgrid"] = fa->mgrid_;
				param_map_["smooth_J_mzeta"] = pd->mzeta_;
				param_map_["smooth_J_MPIrank"] = p->mype_;
				param_map_["smooth_J_MPIsize"] = p->numberpe_;
				SSTMAC_compute_eiger(param_map_, "smooth_J.model");
#endif
#endif

      }

    // diagnostic
    if (iflag == 3 && p->idiag_ == 0)
      {

#ifdef _USE_LOOPS
        libloops_->compute_loop(0, p->mpsi_, 1);

        //!!!efield=sqrt(efield/real(mzeta*sum(mtheta)))/(gyroradius*gyroradius)
        libloops_->compute_fft();

        libloops_->compute_loop(1, p->num_mode_, 2);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["smooth_K_mgrid"] = fa->mgrid_;
				param_map_["smooth_K_MPIrank"] = p->mype_;
				param_map_["smooth_K_MPIsize"] = p->numberpe_;
				param_map_["smooth_K_mpsi"] = p->mpsi_;
				param_map_["smooth_K_mtdiag"] = p->mtdiag_;
				param_map_["smooth_K_mthetamax"] = p->mthetamax_;
				param_map_["smooth_K_mzeta"] = pd->mzeta_;
				SSTMAC_compute_eiger(param_map_, "smooth_K.model");
#endif
#endif

        // ! dominant (n,m) mode history data for flux surface at i=mpsi/2
        int meachtheta = p->mtdiag_ / pd->ntoroidal_;
        icount = meachtheta * p->num_mode_;

				MPI_Gather(NULL, 2 * icount, MPI_DOUBLE, NULL, 2 * icount,
						MPI_DOUBLE, 0, pd->toroidal_comm_);

#ifdef _USE_LOOPS
				if (pd->myrank_toroidal_ == 0)
					{

						libloops_->compute_loop3(1, p->num_mode_, 0, pd->ntoroidal_ - 1, 1,
								meachtheta, 1);
						libloops_->compute_fft();

					}
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["smooth_L_mgrid"] = fa->mgrid_;
				param_map_["smooth_L_MPIrank"] = p->mype_;
				param_map_["smooth_L_MPIsize"] = p->numberpe_;
				param_map_["smooth_L_mpsi"] = p->mpsi_;
				param_map_["smooth_L_mtdiag"] = p->mtdiag_;
				param_map_["smooth_L_mthetamax"] = p->mthetamax_;
				param_map_["smooth_L_mzeta"] = pd->mzeta_;
				SSTMAC_compute_eiger(param_map_, "smooth_L.model");
#endif
#endif
      }

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: done with smooth() \n";
  }

  /**
   * ---------------------------------------------------------------------------------------
   * field
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::field()
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting field() \n";

    // finite difference for e-field in equilibrium unit
    double diffr = 0.5 / fa->deltar_;
    double diffz = 0.5 / pd->deltaz_;
#ifdef _USE_LOOPS
    libloops_->compute_loop(1, p->mpsi_, 1);

    //$omp parallel do private(i,j,k)
    libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 3);

    // d_phi/d_psi
    libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 3);

    // d_phi/d_theta
    //$omp parallel do private(i,k,j,ij,jt)
    libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 3);

    // send phi to right and receive from left
    libloops_->compute_loop(1, fa->mgrid_, 2);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["field_A_mgrid"] = fa->mgrid_;
		param_map_["field_A_MPIrank"] = p->mype_;
		param_map_["field_A_MPIsize"] = p->numberpe_;
		param_map_["field_A_mpsi"] = p->mpsi_;
		param_map_["field_A_mthetamax"] = p->mthetamax_;
		param_map_["field_A_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "field_A.model");
#endif
#endif

    int icount = fa->mgrid_;
    //!idest=mod(mype+1,numberpe)
    int idest = pd->right_pe_;
    int isource = pd->left_pe_;
    int isendtag = pd->myrank_toroidal_;
    int irecvtag = isource;
    MPI_Status istatus;

    MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
        MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

    //send phi to left and receive from right
    //!idest=mod(mype-1+numberpe,numberpe)
    idest = pd->left_pe_;
    //!isource=mod(mype+1,numberpe)
    isource = pd->right_pe_;
    //!isendtag=mype
    isendtag = pd->myrank_toroidal_;
    irecvtag = isource;

    MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
        MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

    // unpack phi_boundary and calculate E_zeta at boundaries, mzeta=1
#ifdef _USE_LOOPS
    //$omp parallel do private(i,j,ii,jt,ij,pleft,pright)
    libloops_->compute_loop(1, fa->mgrid_, 4);

    // adjust the difference between safety factor q and qtinv for fieldline coordinate
    //omp parallel do private(i,j,r,q,delq,ij)
    libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 1);

    // add (0,0) mode, d phi/d psi
    if (p->mode00_ == 1)
      {
        //omp parallel do private(i,j,k,ii,jt,r)
        libloops_->compute_loop2(1, fa->mgrid_, 0, pd->mzeta_, 1);

      }

    // toroidal end point, pack end point (k=mzeta) data
    // send E to right and receive from left
    //$omp parallel do private(i)
    libloops_->compute_loop(1, fa->mgrid_, 3);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["field_B_mgrid"] = fa->mgrid_;
		param_map_["field_B_MPIrank"] = p->mype_;
		param_map_["field_B_MPIsize"] = p->numberpe_;
		param_map_["field_B_mpsi"] = p->mpsi_;
		param_map_["field_B_mthetamax"] = p->mthetamax_;
		param_map_["field_B_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "field_B.model");
#endif
#endif

    icount = 3 * fa->mgrid_;
    idest = pd->right_pe_;
    isource = pd->left_pe_;
    isendtag = pd->myrank_toroidal_;
    irecvtag = isource;

    MPI_Sendrecv(NULL, icount, MPI_DOUBLE, idest, isendtag, NULL, icount,
        MPI_DOUBLE, isource, irecvtag, pd->toroidal_comm_, &istatus);

#ifdef _USE_LOOPS
    // unpack end point data for k=0
    if (pd->myrank_toroidal_ == 0)
      {
        //$omp parallel do private(i,ii,jt)
        libloops_->compute_loop(1, fa->mgrid_, 3);

      }
    else
      {
        //$omp parallel do private(i)
        libloops_->compute_loop(1, fa->mgrid_, 3);

      }

    // poloidal end point
    //omp parallel do private(i,j)
    libloops_->compute_loop2(1, p->mpsi_, 0, pd->mzeta_, 3);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["field_C_mgrid"] = fa->mgrid_;
		param_map_["field_C_MPIrank"] = p->mype_;
		param_map_["field_C_MPIsize"] = p->numberpe_;
		param_map_["field_C_mpsi"] = p->mpsi_;
		param_map_["field_C_mthetamax"] = p->mthetamax_;
		param_map_["field_C_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "field_C.model");
#endif
#endif

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: done with field() \n";
  }

  /**
   * ---------------------------------------------------------------------------------------
   * diagnosis
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::diagnosis(int istep)
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    if (istep == p->ndiag_)
      {
        if (p->mype_ == 0)
          {
            if (p->irun_ == 0)
              {
                //read from disk
              }

#ifdef _USE_LOOPS
            libloops_->compute_loop(1, p->mflux_, 8);
            libloops_->compute_loop(1, p->mpsi_, 8);
#endif

          }
        MPI_Bcast(NULL, 1, MPI_INT, 0, MPI_COMM_WORLD);
      }

    MPI_Reduce(NULL, NULL, 21, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

#ifdef _USE_LOOPS
    libloops_->compute_loop(1, p->mpsi_, 1);
#endif

    if (pd->myrank_partd_ == 0)
      {
        MPI_Reduce(NULL, NULL, 21, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
      }

#ifdef _USE_LOOPS
    if (p->mype_ == 0)
      {

        libloops_->compute_loop(1, p->mpsi_, 1);
      }
#endif
  }

  /**
   * ---------------------------------------------------------------------------------------
   * collision
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::collision(int istep, int neop, int neot, int neoz)
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting collision(" << istep  << ") \n";

    // ! read maxwell.dat
    if (istep == p->ndiag_)
      {
        // ! basic collision time, Braginskii
        // !     tauii=24.0-log(sqrt(0.46e14)/2555.0)
        // !     tauii=2.09e7*(2555.0)**1.5/(0.46e14*tauii*2.31)*sqrt(2.0)/utime
        //    !zeff=2.31

        // ! assume carbon impurity
        //  !!   tauii=23.0-log(sqrt(6.0*zeff*den_electron)/tem_ion**1.5)
        //  !!   tauii=2.09e7*(tem_ion)**1.5*sqrt(aspecie)/(den_electron*tauii*utime*zeff)
        MPI_Bcast(NULL, 100001, MPI_DOUBLE, 0, MPI_COMM_WORLD);

      }

#ifdef _USE_LOOPS
    libloops_->compute_loop(1, fa->mi_, 12);

    libloops_->compute_loop(1, fa->mi_, 27);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["collision_A_mi"] = fa->mi_;
		param_map_["collision_A_MPIrank"] = p->mype_;
		param_map_["collision_A_MPIsize"] = p->numberpe_;
		SSTMAC_compute_eiger(param_map_, "collision_A.model");
#endif
#endif

    // ! global sum
    int icount = neop * neot * neoz;

    MPI_Allreduce(NULL, NULL, icount, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    MPI_Allreduce(NULL, NULL, icount, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    MPI_Allreduce(NULL, NULL, icount, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    // ! local conservation
#ifdef _USE_LOOPS
    libloops_->compute_loop(1, fa->mi_, 11);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["collision_B_mi"] = fa->mi_;
		param_map_["collision_B_MPIrank"] = p->mype_;
		param_map_["collision_B_MPIsize"] = p->numberpe_;
		SSTMAC_compute_eiger(param_map_, "collision_B.model");
#endif
#endif

    //  ! update velocity variables

  }

  /**
   * ---------------------------------------------------------------------------------------
   * poisson
   * @param something
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::poisson(int istep, int iflag)
  {
    //short names
    field_array::ptr fa = field_array_;
    particle_decomp::ptr pd = part_decomp_;
    config_parameters::ptr p = gtcparams_;

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: starting poisson(" << istep      << ") \n";

    int ipartd, nzeta, izeta1, izeta2;

    // ! number of gyro-ring
    int mring = 2;

    // ! number of summation: maximum is 32*mring+1
    int mindex = 32 * mring + 1;
    // !  mindex=53

    // ! gamma=0.75: max. resolution for k=0.577
    double gamma = 0.75;
    int iteration = 5;

    // initialize poisson solver
    if (istep == 1 && p->irk_ == 1 && iflag == 0)
      {
#ifdef _USE_LOOPS
        libloops_->compute_loop4(1, pd->mzeta_, 0, fa->mgrid_, 1, mring, 1, 8, 29);
        libloops_->compute_loop2(1, pd->mzeta_, 0, fa->mgrid_, 3);
#else
#ifdef _USE_EIGER
				param_map_.clear();
				param_map_["poisson_A_mgrid"] = fa->mgrid_;
				param_map_["poisson_A_MPIrank"] = p->mype_;
				param_map_["poisson_A_MPIsize"] = p->numberpe_;
				param_map_["poisson_A_mpsi"] = p->mpsi_;
				param_map_["poisson_A_mzeta"] = pd->mzeta_;
				param_map_["poisson_A_mindex"] = mindex;
				SSTMAC_compute_eiger(param_map_, "poisson_A.model");
#endif
#endif
      }

    if (p->npartdom_ > 1 && (pd->mzeta_ % p->npartdom_) == 0)
      {
        //! mzeta is a multiple of npartdom so we can split the work of the
        //! k=1,mzeta loop between the processors having the same toroidal
        //! domain but a different particle domain.
        //! First we split mzeta into npartdom and determine the loop indices
        //! for each process part of the particle domain.
        ipartd = 1;
        nzeta = pd->mzeta_ / p->npartdom_;
        izeta1 = pd->myrank_partd_ * nzeta + 1;
        izeta2 = (pd->myrank_partd_ + 1) * nzeta;
      }
    else
      {
        ipartd = 0;
        izeta1 = 1;
        izeta2 = pd->mzeta_;
      }

    //   !!!do k=1,mzeta
#ifdef _USE_LOOPS
    libloops_->compute_loop2(izeta1, izeta2, 1, fa->mgrid_, 1);
    libloops_->compute_loop2(2, iteration, 1, fa->mgrid_, 4);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["poisson_B_mgrid"] = fa->mgrid_;
		param_map_["poisson_B_mindex"] = mindex;
		param_map_["poisson_B_MPIrank"] = p->mype_;
		param_map_["poisson_B_MPIsize"] = p->numberpe_;
		param_map_["poisson_B_mpsi"] = p->mpsi_;
		param_map_["poisson_B_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "poisson_B.model");
#endif
#endif

    if (ipartd == 1)
      {
        //   ! Since the work was split between the processors of different particle
        //   ! domains, we need to gather the full array phi on these processors.

#ifdef _USE_LOOPS
        libloops_->compute_loop2(izeta1, izeta2, 1, fa->mgrid_, 1);
#endif

        MPI_Allgather(NULL, nzeta * fa->mgrid_, MPI_DOUBLE, NULL,
            nzeta * fa->mgrid_, MPI_DOUBLE, pd->partd_comm_);

#ifdef _USE_LOOPS
        libloops_->compute_loop2(1, pd->mzeta_, 1, fa->mgrid_, 1);
#endif

      }

#ifdef _USE_LOOPS
    // ! in equilibrium unit
    //  !$omp parallel do private(i)
    libloops_->compute_loop2(0, p->mpsi_, 1, pd->mzeta_, 2);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["poisson_C_mgrid"] = fa->mgrid_;
		param_map_["poisson_C_MPIrank"] = p->mype_;
		param_map_["poisson_C_MPIsize"] = p->numberpe_;
		param_map_["poisson_C_mindex"] = mindex;
		param_map_["poisson_C_mpsi"] = p->mpsi_;
		param_map_["poisson_C_mzeta"] = pd->mzeta_;
		SSTMAC_compute_eiger(param_map_, "poisson_C.model");
#endif
#endif

    //SSTMAC_DEBUG << "gtc(" << p->mype_ << "):: done with poisson \n";

  }

  void
  gtc_main::snapshot()
  {
    //SSTMAC_DEBUG << "gtc(" << gtcparams_->mype_ << "):: snapshot \n";
    int mbin_psi = 5;
    int mbin_u = 41;

#ifdef _USE_LOOPS
    libloops_->compute_loop(1, field_array_->mi_, 24);
#else
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["snapshot_A_mi"] = field_array_->mi_;
		param_map_["snapshot_A_me"] = field_array_->me_;
		param_map_["snapshot_A_MPIrank"] = gtcparams_->mype_;
		param_map_["snapshot_A_MPIsize"] = gtcparams_->numberpe_;
		SSTMAC_compute_eiger(param_map_, "snapshot_A.model");
#endif
#endif

    int icount = mbin_psi * mbin_u;
    MPI_Reduce(NULL, NULL, icount, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, icount, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, icount, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, icount, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, icount, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, icount, MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, gtcparams_->mpsi_, MPI_FLOAT, MPI_SUM, 0,
        MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, gtcparams_->mpsi_, MPI_FLOAT, MPI_SUM, 0,
        MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, gtcparams_->mpsi_, MPI_FLOAT, MPI_SUM, 0,
        MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, gtcparams_->mpsi_, MPI_FLOAT, MPI_SUM, 0,
        MPI_COMM_WORLD);
    MPI_Reduce(NULL, NULL, gtcparams_->mpsi_, MPI_FLOAT, MPI_SUM, 0,
        MPI_COMM_WORLD);

    int jm = field_array_->mtheta_[gtcparams_->mpsi_ / 2];
    int count = jm * part_decomp_->mzeta_;
    MPI_Gather(NULL, count, MPI_FLOAT, NULL, count, MPI_FLOAT, 0,
        part_decomp_->toroidal_comm_);
#ifdef _USE_EIGER
		if(gtcparams_->mype_ == 0){
			param_map_.clear();
			param_map_["snapshot_B_mpsi"] = gtcparams_->mpsi_;
			param_map_["snapshot_B_jm"] = jm;
			param_map_["snapshot_B_MPIrank"] = gtcparams_->mype_;
			param_map_["snapshot_B_MPIsize"] = gtcparams_->numberpe_;
			SSTMAC_compute_eiger(param_map_, "snapshot_B.model");
		}
#endif

    //SSTMAC_DEBUG << "gtc(" << gtcparams_->mype_ << "):: done with snapshot \n";
  }
}