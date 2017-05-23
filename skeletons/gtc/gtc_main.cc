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
#define _USE_MATH_DEFINES
#include <cmath>
#include <functional>
#include <numeric>

#include <sstmac/sstmacro.h>

namespace gtc {

  using namespace sstmac;
  using namespace sstmac::sw;

  gtc_main::gtc_main(sprockit::sim_parameters* params, software_id sid,
                     sstmac::sw::operating_system* os) :
    app(params, sid, os)
  {
#ifdef _USE_LOOPS
      libloops_ = new sstmac::sw::lib_compute_loops(params, sid, os_);
#endif
      /**
          sstobject {
              name = Params;
              gui = GTC;
              docstring = The configuration parameters object used by GTC;
              type = gtc::config_parameters;
              tab = App;
              alias = GTC Parameters;
          }
      */
    gtcparams_ = config_parameters::construct(params);

    nthreads_ = params->get_optional_int_param("gtc_main_nthreads", 1);

    gtcparams_->ihybrid_ = 1;

  }

  /**
   * ---------------------------------------------------------------------------------------
   * setup
   *     : sets up the simulation, modifies the parameters as necessary
   * ---------------------------------------------------------------------------------------
   */

  void
  gtc_main::setup()
  {

    // total # of PE and rank of PE
    MPI_Comm_size(MPI_COMM_WORLD, &gtcparams_->numberpe_);
    MPI_Comm_rank(MPI_COMM_WORLD, &gtcparams_->mype_);

    // Read the input file that contains the run parameters

    read_input_params();

    //  Changing the units of a0 and a1 from units of "a" to units of "R_0"
    gtcparams_->a0_ = gtcparams_->a0_ * gtcparams_->a_;
    gtcparams_->a1_ = gtcparams_->a1_ * gtcparams_->a_;

    // numerical constant
    gtcparams_->mstep_ = std::max(2, gtcparams_->mstep_);
    gtcparams_->msnap_ = std::min(gtcparams_->msnap_,
        gtcparams_->mstep_ / gtcparams_->ndiag_);
    int isnap = gtcparams_->mstep_ / gtcparams_->msnap_;
    gtcparams_->idiag1_ = gtcparams_->mpsi_ / 2;
    gtcparams_->idiag2_ = gtcparams_->mpsi_ / 2;
    if (gtcparams_->nonlinear_ <= 0)
      {
        gtcparams_->paranl_ = 0.0;
        gtcparams_->mode00_ = 0;
        gtcparams_->idiag1_ = 1;
        gtcparams_->idiag2_ = gtcparams_->mpsi_;
      }
    gtcparams_->rc_ = gtcparams_->rc_ * (gtcparams_->a0_ + gtcparams_->a1_);
    gtcparams_->rw_ = 1.0 / (gtcparams_->rw_ * (gtcparams_->a1_
        - gtcparams_->a0_));

    // Set up the particle decomposition within each toroidal domain
    part_decomp_ = particle_decomp::construct();
    part_decomp_->setup(gtcparams_);

    // equilibrium unit: length (unit=cm) and time (unit=second) unit
    double ulength = gtcparams_->r0_;
    double utime = 1.0 / (9580.0 * gtcparams_->b0_); // time unit = inverse gyrofrequency of proton

    // primary ion thermal gyroradius in equilibrium unit, vthermal=sqrt(T/m)
    gtcparams_->gyroradius_ = 102.0 * sqrt(
        gtcparams_->aion_ * gtcparams_->temperature_) / (std::abs(gtcparams_->qion_)
        * gtcparams_->b0_) / ulength;
    double tstep = gtcparams_->tstep_ * gtcparams_->aion_ / (std::abs(
        gtcparams_->qion_) * gtcparams_->gyroradius_ * gtcparams_->kappati_);

    // basic ion-ion collision time, Braginskii definition
    gtcparams_->do_collision_ = false;
    double r, q, zeff, tau_vth;

    if (gtcparams_->tauii_ > 0)
      {
        //tau_vth=24.0_wp-log(sqrt(edensity0)/temperature) ! Coulomb logarithm
        //tau_vth=2.09e7_wp*(temperature)**1.5_wp/(edensity0*tau_vth*2.31_wp)*sqrt(2.0_wp)/utime
        r = 0.5 * (gtcparams_->a0_ + gtcparams_->a1_);
        q = gtcparams_->q0_ + gtcparams_->q1_ * r / gtcparams_->a_
            + gtcparams_->q2_ * r * r / (gtcparams_->a_ * gtcparams_->a_);
        zeff = gtcparams_->qion_; // Set zeff to main ion species

        tau_vth = 23.0 - log(
            sqrt(zeff * zeff * gtcparams_->edensity0_) / pow(
                gtcparams_->temperature_, 1.5));
        tau_vth = 2.09e7 * pow(gtcparams_->temperature_, 1.5) * sqrt(
            gtcparams_->aion_) / (gtcparams_->edensity0_ * tau_vth * utime
            * zeff);
        gtcparams_->tauii_ = tau_vth * gtcparams_->tauii_; // Multiply v_thermal tau by scaling factor tauii
        gtcparams_->do_collision_ = true;
        if (gtcparams_->mype_ == 0)
          {
            //SSTMAC_DEBUG << "Collision time tauii=" << gtcparams_->tauii_
            //    << "  nu_star=" << q / (gtcparams_->tauii_
            //    * gtcparams_->gyroradius_ * pow(r, 1.5)) << "  q=" << q << "\n";
          }

      }

    field_array_ = field_array::construct(gtcparams_->mpsi_);
    field_array_->setup(gtcparams_, gtcparams_->gyroradius_, part_decomp_);

    int nparam;
    if (gtcparams_->track_particles_ == 1)
      {
        // We keep track of the particles by tagging them with a number
        // We add an extra element to the particle array, which will hold
        // the particle tag, i.e. just a number
        // Each processor has its own "ptracked" array to accumulate the tracked
        // particles that may or may not reside in its subdomain.
        // The vector "ntrackp" keeps contains the number of tracked particles
        // currently residing on the processor at each time step. We write out
        // the data to file every (mstep/msnap) steps.
        gtcparams_->nparam_ = 7;

      }
    else
      {
        // No tagging of the particles
        gtcparams_->nparam_ = 6;
      }
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["setup_A_mpsi"] = gtcparams_->mpsi_;
		param_map_["setup_A_mgrid"] = field_array_->mgrid_;
		param_map_["setup_A_mzeta"] = part_decomp_->mzeta_;
		param_map_["setup_A_mthetamax"] = gtcparams_->mthetamax_;
		param_map_["setup_A_mi"] = field_array_->mi_;
		param_map_["setup_A_me"] = field_array_->me_;
		param_map_["setup_A_MPIrank"] = gtcparams_->mype_;
		param_map_["setup_A_MPIsize"] = gtcparams_->numberpe_;
		SSTMAC_compute_eiger(param_map_, "setup_A.model");

#endif

  }

  /**
   * ---------------------------------------------------------------------------------------
   * read_input_params
   *    : models reading the parameter file from disk
   * ---------------------------------------------------------------------------------------
   */

  void
  gtc_main::read_input_params()
  {
    int n_integers = 19;
    int n_reals = 27;
    MPI_Bcast(NULL, n_integers, MPI_INT, 0, MPI_COMM_WORLD);
    MPI_Bcast(NULL, n_reals, MPI_DOUBLE, 0, MPI_COMM_WORLD);
  }

  /**
   * ---------------------------------------------------------------------------------------
   * rand_num_gen_init
   *    : initializes the random number generators
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::rand_num_gen_init()
  {

    int i, m, nsize;
    std::vector<int> mget, mput;

    if (gtcparams_->rng_control_ <= 0)
      {
        // **** Use the intrinsic F90 random number generator *****
        // initialize f90 random number generator
        rng_ = RNG::SHR3::construct();

      }
    else
      {
        // All the processors start with the same seed.
        rng_ = RNG::SHR3::construct(gtcparams_->rng_control_);

      }

  }

  /**
   * ---------------------------------------------------------------------------------------
   * load
   *    : initializes the values for the computation
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::load()
  {

  }

  /**
   * ---------------------------------------------------------------------------------------
   * skeleton_main
   * ---------------------------------------------------------------------------------------
   */
  void
  gtc_main::skeleton_main()
  {
    mpi_ = get_api<sumi::mpi_api>();
    /* use global_parameters
     use particle_array
     use particle_tracking
     use field_array
     use diagnosis_array
     implicit none*/

    int i, ierror;

    MPI_Init(NULL, NULL);

    int rank = mpi()->comm_world()->rank();
    //SSTMAC_DEBUG << "gtc (" << rank << "): Done with MPI_Init \n";

    // input parameters, setup equilibrium, allocate memory
    setup();

    //SSTMAC_DEBUG << "gtc (" << rank << "): Done with setup \n";

    // initialize particle position and velocity
    load();
#ifdef _USE_EIGER
		param_map_.clear();
		param_map_["load_A_mi"] = field_array_->mi_;
		param_map_["load_A_MPIrank"] = gtcparams_->mype_;
		param_map_["load_A_MPIsize"] = gtcparams_->numberpe_;
		SSTMAC_compute_eiger(param_map_, "load_A.model");
#endif

    //SSTMAC_DEBUG << "gtc (" << rank << "): Done with load\n";

    // Write out initial position of tracked particles
    //!!  if(track_particles==1)then
    //!!     call locate_tracked_particles
    //!!     call write_tracked_particles
    //!!  endif

    int istep = 0;

    bool do_checkpoint = true;

    //calculate ion gather-scatter coefficients
    /* call
     timer(t0, dt, t0wc, dtwc)
     time(7) = time(7) + dt
     timewc(7) = timewc(7) + dtwc
     loop_time = t0wc */

    // main time loop
    istep = 1;

    for (; istep <= gtcparams_->mstep_; istep++)
      {

        for (int irk = 1; irk <= 2; irk++)
          {

            // idiag=0: do time history diagnosis
            gtcparams_->idiag_ = ((irk + 1) % 2) + (istep % gtcparams_->ndiag_);
            gtcparams_->irk_ = irk;

            // smooth potential, diagnostics
            smooth(3);

            ///  timer(t0, dt, t0wc, dtwc)
            ///  time(5) = time(5) + dt
            ///  timewc(5) = timewc(5) + dtwc

            // field
            field();

            ///  timer(t0, dt, t0wc, dtwc)
            ///  time(6) = time(6) + dt
            ///  timewc(6) = timewc(6) + dtwc

            // push ion
            pushi(istep);

            if (gtcparams_->idiag_ == 0)
              {
                diagnosis(istep);
                //!!  CALL VOLUME    !Original netCDF 3D potential data
                //!! //CALL OUTPUT3D  //HDF5 parallel output of 3D potential data
                //!!  CALL DATAOUT   //New version of netCDF 3D potential data
              }

            /// call
            ///  timer(t0, dt, t0wc, dtwc)
            ///  time(1) = time(1) + dt
            ///  timewc(1) = timewc(1) + dtwc

            // redistribute ion across PEs
            shifti();

            ///  timer(t0, dt, t0wc, dtwc)
            ///  time(2) = time(2) + dt
            ///   timewc(2) = timewc(2) + dtwc

            // collisions
            //if(irk==2 .and. do_collision)CALL LORENTZ_COLL
            if (irk == 2 && gtcparams_->do_collision_)
              {
                collision(istep);
              }

            ///call
            ///   timer(t0, dt, t0wc, dtwc)
            ///   time(9) = time(9) + dt
            ///   timewc(9) = timewc(9) + dtwc

            // ion perturbed density
            chargei(istep);

            ///   timer(t0, dt, t0wc, dtwc)
            ///   time(3) = time(3) + dt
            ///   timewc(3) = timewc(3) + dtwc

            // smooth ion density
            smooth(0);

            ///   timer(t0, dt, t0wc, dtwc)
            ///   time(5) = time(5) + dt
            ///   timewc(5) = timewc(5) + dtwc

            // solve GK Poisson equation using adiabatic electron
            poisson(istep, 0);

            ///   timer(t0, dt, t0wc, dtwc)
            ///   time(4) = time(4) + dt
            ///   timewc(4) = timewc(4) + dtwc

            for (gtcparams_->ihybrid_ = 1; gtcparams_->ihybrid_
                <= gtcparams_->nhybrid_; gtcparams_->ihybrid_++)
              {
                // smooth potential
                smooth(2);

                ///   timer(t0, dt, t0wc, dtwc)
                ///   time(5) = time(5) + dt
                ///   timewc(5) = timewc(5) + dtwc

                // push electron, sub-cycling
                for (int i = 1; i <= gtcparams_->ncycle_ * irk; i++)
                  {
                    // 1st RK step

                    pushe(i, 1, istep);

                    ///   timer(t0, dt, t0wc, dtwc)
                    ///   time(1) = time(1) + dt
                    ///   timewc(1) = timewc(1) + dtwc

                    shifte();
                    ///   timer(t0, dt, t0wc, dtwc)
                    ///   time(2) = time(2) + dt
                    ///   timewc(2) = timewc(2) + dtwc

                    // 2nd RK step

                    pushe(i, 2, istep);

                    ///  timer(t0, dt, t0wc, dtwc)
                    ///  time(1) = time(1) + dt
                    ///  timewc(1) = timewc(1) + dtwc

                    shifte();

                    ///  timer(t0, dt, t0wc, dtwc)
                    ///   time(2) = time(2) + dt
                    ///  timewc(2) = timewc(2) + dtwc
                  }

                // nonadiabatic electron charge density
                chargee();

                ///   call
                ///   timer(t0, dt, t0wc, dtwc)
                ///   time(3) = time(3) + dt
                ///   timewc(3) = timewc(3) + dtwc

                // smooth electron density
                smooth(1);

                ///   call
                ///   timer(t0, dt, t0wc, dtwc)
                ///   time(5) = time(5) + dt
                ///   timewc(5) = timewc(5) + dtwc

                // solve GK Poisson equation using non-adiabatic electron
                poisson(istep, 1);

                ///    call
                ///    timer(t0, dt, t0wc, dtwc)
                ///    time(4) = time(4) + dt
                ///    timewc(4) = timewc(4) + dtwc
              }
          }

        /*    call
         timer(tr0, dt, tr0wc, dtwc)
         if(track_particles==1 .and. nptrack>0)
         call
         locate_tracked_particles
         call
         timer(tr0, dt, tr0wc, dtwc)
         tracktcpu = tracktcpu + dt
         tracktwc = tracktwc + dtwc
        */

         // profile snapshots, write particle information to restart file
         if(istep % (gtcparams_->mstep_ / gtcparams_->msnap_) ==  0)
          {
            snapshot();
          }
         //if(track_particles==1 .and. nptrack>0)
         //call
         //write_tracked_particles
         //endif
      }

    /*
     call
     timer(t0, dt, t0wc, dtwc)
     loop_time = t0wc - loop_time
     time(8) = t0 - time(8)
     timewc(8) = t0wc - timewc(8)
     ic(1) = 'pusher'
     ic(2) = 'shift'
     ic(3) = 'charge'
     ic(4) = 'poisson'
     ic(5) = 'smooth'
     ic(6) = 'field'
     ic(7) = 'load'
     ic(8) = 'total'
     if(mype==0)
     then
     write  (stdout,*)'CPU TIME USAGE (in SEC):'
     write(stdout,*)ic
     write(stdout,'(8(1pe10.3),/)')time(1:8)
     write(stdout,*)'WALL CLOCK TIMES (in SEC):'
     write(stdout,*)ic
     write(stdout,'(8(1pe10.3))')timewc(1:8)
     write(stdout,'("TOTAL CPU TIME USAGE (SEC):",f12.3)')time(8)
     write(stdout,'("TOTAL WALL CLOCK TIME(SEC):",f12.3)')timewc(8)
     write(stdout,'("CPU+WALL TIMES IN COLL(SEC):",2f12.3)')time(9),timewc(9)
     write(stdout,'("NERSC TIME(SEC):",f12.3)')loop_time
     if(track_particles==1)write(stdout,'("PARTICLE TRACKING TIME(SEC):",f12.3)')tracktwc
     endif
     */

    /*
     #ifdef __NERSC
     //  call system_stats()
     #endif
     */

    MPI_Finalize();

  }

} // end of namespace sstmac.