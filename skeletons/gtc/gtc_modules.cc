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

#include "gtc_modules.h"

#define _USE_MATH_DEFINES
#include <math.h>
#include <functional>
#include <numeric>

namespace gtc
{

  config_parameters::config_parameters(sprockit::sim_parameters* params)
  {
    bool err = false;
    irun_ = params->get_int_param("gtc_main_irun");
    mstep_ = params->get_int_param("gtc_main_mstep");
    msnap_ = params->get_int_param("gtc_main_msnap");
    ndiag_ = params->get_int_param("gtc_main_ndiag");
    nonlinear_ = params->get_double_param("gtc_main_nonlinear");
    paranl_ = params->get_double_param("gtc_main_paranl");
    mode00_ = params->get_int_param("gtc_main_mode00");
    tstep_ = params->get_double_param("gtc_main_tstep");
    micell_ = params->get_int_param("gtc_main_micell");
    mecell_ = params->get_int_param("gtc_main_mecell");
    mpsi_ = params->get_int_param("gtc_main_mpsi");
    mthetamax_ = params->get_int_param("gtc_main_mthetamax");
    mzetamax_ = params->get_int_param("gtc_main_mzetamax");
    npartdom_ = params->get_int_param("gtc_main_npartdom");
    ncycle_ = params->get_int_param("gtc_main_ncycle");
    a_ = params->get_double_param("gtc_main_a");
    a0_ = params->get_double_param("gtc_main_a0");
    a1_ = params->get_double_param("gtc_main_a1");
    q0_ = params->get_double_param("gtc_main_q0");
    q1_ = params->get_double_param("gtc_main_q1");
    q2_ = params->get_double_param("gtc_main_q2");
    rc_ = params->get_double_param("gtc_main_rc");
    rw_ = params->get_double_param("gtc_main_rw");
    aion_ = params->get_double_param("gtc_main_aion");
    qion_ = params->get_double_param("gtc_main_qion");
    aelectron_ = params->get_double_param("gtc_main_aelectron");
    qelectron_ = params->get_double_param("gtc_main_qelectron");
    kappati_ = params->get_double_param("gtc_main_kappati");
    kappate_ = params->get_double_param("gtc_main_kappate");
    kappan_ = params->get_double_param("gtc_main_kappan");
    fixed_Tprofile = params->get_optional_bool_param("gtc_main_fixed_Tprofile",
        true);
    tite_ = params->get_double_param("gtc_main_tite");
    flow0_ = params->get_double_param("gtc_main_flow0");
    flow1_ = params->get_double_param("gtc_main_flow1");
    flow2_ = params->get_double_param("gtc_main_flow2");
    r0_ = params->get_double_param("gtc_main_r0");
    b0_ = params->get_double_param("gtc_main_b0");
    temperature_ = params->get_double_param("gtc_main_temperature");
    edensity0_ = params->get_double_param("gtc_main_edensity0");
    nbound_ = params->get_int_param("gtc_main_nbound");
    umax_ = params->get_double_param("gtc_main_umax");
    iload_ = params->get_int_param("gtc_main_iload");
    tauii_ = params->get_double_param("gtc_main_tauii");
    track_particles_ = params->get_bool_param("gtc_main_track_particles");
    nptrack_ = params->get_int_param("gtc_main_nptrack");
    rng_control_ = params->get_long_param("gtc_main_rng_control");
    params->get_vector_param("gtc_main_nmode", nmode_);
    params->get_vector_param("gtc_main_mmode", mmode_);

    nhybrid_ = params->get_optional_int_param("gtc_main_nhybrid", 0);

    num_mode_ = nmode_.size();
    mflux_ = 5;
    m_poloidal_ = 9;
  }

  field_array::field_array(int mpsi)
  {

  }

  void
  field_array::setup(const config_parameters::ptr& params, double gyroradius,
      const particle_decomp::ptr& partdecomp)
  {

    // --- Define poloidal grid ---
    // grid spacing
    deltar_ = (params->a1_ - params->a0_) / double(params->mpsi_);

    mtheta_.resize(params->mpsi_ + 1);
    // grid shift associated with fieldline following coordinates
    double tdum = 2.0 * M_PI * params->a1_ / double(params->mthetamax_);
    double r, q;
    for (int i = 0; i <= params->mpsi_; i++)
      {
        r = params->a0_ + deltar_ * double(i);
        mtheta_[i] = std::max(2,
            std::min(params->mthetamax_, 2 * int(M_PI * r / tdum + 0.5))); // even # poloidal grid

      }
    // un-comment the next two lines to use magnetic coordinate
    //  qtinv=0.0
    //  itran=0

    // When doing mode diagnostics, we need to switch from the field-line following
    // coordinates alpha-zeta to a normal geometric grid in theta-zeta. This
    // translates to a greater number of grid points in the zeta direction, which
    // is mtdiag. Precisely, mtdiag should be mtheta/q but since mtheta changes
    // from one flux surface to another, we use a formula that gives us enough
    // grid points for all the flux surfaces considered.
    params->mtdiag_ = (params->mthetamax_ / params->mzetamax_)
        * params->mzetamax_;
    // In the case of a non-linear run, we need only half the points.
    if (params->nonlinear_ > 0.5)
      params->mtdiag_ = params->mtdiag_ / 2;

    // number of grids on a poloidal plane
    mgrid_ = std::accumulate(mtheta_.begin(), mtheta_.end(), 1);
    mi_local_ = params->micell_ * (mgrid_ - params->mpsi_) * partdecomp->mzeta_; //# of ions in toroidal domain
    mi_ = params->micell_ * (mgrid_ - params->mpsi_) * partdecomp->mzeta_
        / params->npartdom_; //# of ions per processor
    if (mi_ < ((int) mi_local_ % params->npartdom_))
      mi_ = mi_ + 1;
    me_local_ = params->mecell_ * (mgrid_ - params->mpsi_) * partdecomp->mzeta_; //# of electrons in toroidal domain
    me_ = params->mecell_ * (mgrid_ - params->mpsi_) * partdecomp->mzeta_
        / params->npartdom_; //# of electrons per processor
    if (me_ < ((int) me_local_ % params->npartdom_))
      me_ = me_ + 1;
    mimax_ = mi_ + 100 * ceil(sqrt(double(mi_))); // ions array upper bound
    memax_ = me_ + 100 * ceil(sqrt(double(me_))); // electrons array upper bound

    if (params->mype_ == 0)
      {
        //SSTMAC_DEBUG << "#ions per proc= " << mi_ << " #electrons per proc= " << me_ << "\n";
        //SSTMAC_DEBUG << "#ions per grid cell= " << params->micell_ << "   #electrons per grid cell= " << params->mecell_ << "\n";
        //SSTMAC_DEBUG << "#ions per toroidal domain=" << mi_local_ << "   #electrons per toroidal domain= " << me_local_ << "\n";
        //SSTMAC_DEBUG << "mzeta=" << partdecomp->mzeta_ << "\n";
        //SSTMAC_DEBUG << "mgrid=" << mgrid_ << "\n";
        //SSTMAC_DEBUG << "mpsi=" << params->mpsi_ << "\n";

      }

  }

  particle_decomp::particle_decomp()
  {
  }

  void
  particle_decomp::setup(const config_parameters::ptr& params)
  {
    //SSTMAC_DEBUG << "gtc(" << params->mype_ << ")::particle_decomp - starting..\n";
    // ----- First we verify the consistency of ntoroidal and npartdom -------
    // The number of toroidal domains (ntoroidal) times the number of particle
    // "domains" (npartdom) needs to be equal to the number of processor "numberpe".
    // numberpe cannot be changed since it is given on the command line.

    // numberpe must be a multiple of npartdom so change npartdom accordingly
    while (params->numberpe_ % params->npartdom_ != 0)
      {
        params->npartdom_ = params->npartdom_ - 1;
        if (params->npartdom_ == 1)
          break;
      }
    ntoroidal_ = params->numberpe_ / params->npartdom_;
    if (params->mype_ == 0)
      {
        //SSTMAC_DEBUG << "******************************************************* \n";
        //SSTMAC_DEBUG << "  Using npartdom=" << params->npartdom_
        //      << " and ntoroidal=" << ntoroidal_ << "\n";
        //SSTMAC_DEBUG << "******************************************************* \n";

      }

    // make sure that mzetamax is a multiple of ntoroidal
    params->mzetamax_ = ntoroidal_ * std::max(1,
        int(double(params->mzetamax_) / double(ntoroidal_) + 0.5));

    // Make sure that "mpsi", the total number of flux surfaces, is an even
    // number since this quantity will be used in Fast Fourier Transforms
    params->mpsi_ = 2 * (params->mpsi_ / 2);

    // We now give each PE (task) a unique domain identified by 2 numbers: the
    // particle and toroidal domain numbers.
    //    particle_domain_location = rank of the particle domain holding mype
    //    toroidal_domain_location = rank of the toroidal domain holding mype
    //
    // On the IBM SP, the MPI tasks are distributed in an orderly fashion to each
    // node unless the LoadLeveler instruction "#@ blocking = unlimited" is used.
    // On Seaborg for example, the first 16 tasks (mype=0-15) will be assigned to
    // the first node that has been allocated to the job, then the next 16
    // (mype=16-31) will be assigned to the second node, etc. When not using the
    // OpenMP, we want the particle domains to sit on the same node because
    // communication is more intensive. To achieve this, successive PE numbers are
    // assigned to the particle domains first.
    // It is easy to achieve this ordering by simply using mype/npartdom for
    // the toroidal domain and mod(mype,npartdom) for the particle domain.
    //
    //  pe_number=0
    //  do j=0,ntoroidal-1
    //     do i=0,npartdom-1
    //        pe_grid(i,j)=pe_number
    //        particle_domain_location(pe_number)=i
    //        toroidal_domain_location(pe_number)=j
    //        pe_number=pe_number+1
    //     enddo
    //  enddo

    particle_domain_location_ = (params->mype_ % params->npartdom_);
    toroidal_domain_location_ = params->mype_ / params->npartdom_;

    //  write(0,*)'mype=',mype,"  particle_domain_location =",&
    //            particle_domain_location,' toroidal_domain_location =',&
    //            toroidal_domain_location,' pi=',pi

    // Domain decomposition in toroidal direction.
    mzeta_ = params->mzetamax_ / ntoroidal_;
    zetamin_ = 2.0 * M_PI * double(toroidal_domain_location_) / double(
        ntoroidal_);
    zetamax_ = 2.0 * M_PI * double(toroidal_domain_location_ + 1) / double(
        ntoroidal_);

    //  write(0,*)mype,' in set_particle_decomp: mzeta=',mzeta,'  zetamin=',&
    //            zetamin,'  zetamax=',zetamax

    // grid spacing in the toroidal direction
    deltaz_ = (zetamax_ - zetamin_) / double(mzeta_);

    // ---- Create particle domain communicator and toroidal communicator -----
    // We now need to create a new communicator which will include only the
    // processes located in the same toroidal domain. The particles inside
    // each toroidal domain are divided equally between "npartdom" processes.
    // Each one of these processes will do a charge deposition on a copy of
    // the same grid, requiring a toroidal-domain-wide reduction after the
    // deposition. The new communicator will allow the reduction to be done
    // only between those processes located in the same toroidal domain.
    //
    // We also need to create a purely toroidal communicator so that the
    // particles with the same particle domain id can exchange with their
    // toroidal neighbors.
    //
    // Form 2 subcommunicators: one that includes all the processes located in
    // the same toroidal domain (partd_comm), and one that includes all the
    // processes part of the same particle domain (toroidal_comm).
    // Here is how to create a new communicator from an old one by using
    // the MPI call "MPI_COMM_SPLIT()".
    // All the processes passing the same value of "color" will be placed in
    // the same communicator. The "rank_in_new_comm" value will be used to
    // set the rank of that process on the communicator.
    //  call MPI_COMM_SPLIT(old_comm,color,rank_in_new_comm,new_comm,ierror)

    // particle domain communicator (for communications between the particle
    // domains WITHIN the same toroidal domain)

    if (params->npartdom_ > 1)
      {

        MPI_Comm_split(MPI_COMM_WORLD, toroidal_domain_location_,
            particle_domain_location_, &partd_comm_);

        // toroidal communicator (for communications BETWEEN toroidal domains of same
        // particle domain number)
        MPI_Comm_split(MPI_COMM_WORLD, particle_domain_location_,
            toroidal_domain_location_, &toroidal_comm_);
      }
    else
      {
        toroidal_comm_ = MPI_COMM_WORLD;
        partd_comm_ = MPI_COMM_SELF;
      }

    MPI_Comm_size(partd_comm_, &nproc_partd_);
    MPI_Comm_rank(partd_comm_, &myrank_partd_);

    MPI_Comm_size(toroidal_comm_, &nproc_toroidal_);
    MPI_Comm_rank(toroidal_comm_, &myrank_toroidal_);

    //  write(0,*)'mype=',mype,'  nproc_toroidal=',nproc_toroidal,&
    //       ' myrank_toroidal=',myrank_toroidal,'  nproc_partd=',nproc_partd,&
    //       ' myrank_partd=',myrank_partd

    if (nproc_partd_ != params->npartdom_)
      {
        std::cout << "*** nproc_partd= " << nproc_partd_
            << " NOT EQUAL to npartdom=" << params->npartdom_ << "\n";
        throw sprockit::spkt_error(
            "gtc_main::set_particle_decomp() - parameter error 1");
      }

    if (nproc_toroidal_ != ntoroidal_)
      {
        std::cout << "*** nproc_toroidal=" << nproc_toroidal_
            << " NOT EQUAL to ntoroidal=" << ntoroidal_ << "\n";
        throw sprockit::spkt_error(
            "gtc_main::set_particle_decomp() - parameter error 2");
      }

    // We now find the toroidal neighbors of the current toroidal domain and
    // store that information in 2 easily accessible variables. This information
    // is needed several times inside the code, such as when particles cross
    // the domain boundaries. We will use the toroidal communicator to do these
    // transfers so we don't need to worry about the value of myrank_partd.
    // We have periodic boundary conditions in the toroidal direction so the
    // neighbor to the left of myrank_toroidal=0 is (ntoroidal-1).

    left_pe_ = ((myrank_toroidal_ - 1 + ntoroidal_) % ntoroidal_);
    right_pe_ = ((myrank_toroidal_ + 1) % ntoroidal_);

    //SSTMAC_DEBUG << "******** decomposition ************** \n";
    //SSTMAC_DEBUG << "toroidal_domain_loc: " << toroidal_domain_location_ << "\n";
    //SSTMAC_DEBUG << "particle_domain_loc: " << particle_domain_location_ << "\n";
    //SSTMAC_DEBUG << "npartdom: " << params->npartdom_ << "\n";
    //SSTMAC_DEBUG << "myrank_toroidal: " << myrank_toroidal_ << "\n";
    //SSTMAC_DEBUG << "ntoroidal: " << ntoroidal_ << "\n";
    //SSTMAC_DEBUG << "left_pe: " << left_pe_ << "\n";
    //SSTMAC_DEBUG << "right_pe: " << right_pe_ << "\n";
    //SSTMAC_DEBUG << "************************************* \n";
    //SSTMAC_DEBUG << "gtc(" << params->mype_ << ")::particle_decomp - done \n";
  }

} /* namespace sstmac */