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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_GTC_GTC_MODULES_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_GTC_GTC_MODULES_H_INCLUDED

#include <mpi.h>
#include <sstmac/common/rng.h>
#include <sprockit/debug.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/ptr_type.h>

namespace gtc
{

#define SMALL 1e-20
#define BIG 1e20;

  /**
   * Base type for modules that encapsulate variables (a la fortran)
   */
  class gtc_module : public sprockit::ptr_type
  {

  public:
    typedef sprockit::refcount_ptr<gtc_module> ptr;

    gtc_module()
    {
    }

  };

  /**
   * Holds all the parameters that come from the config file
   */
  class config_parameters : public gtc_module
  {

  protected:
    config_parameters(sprockit::sim_parameters* params);

  public:
    typedef sprockit::refcount_ptr<config_parameters> ptr;

    static ptr
    construct(sprockit::sim_parameters* params)
    {
      return new config_parameters(params);
    }

    virtual
    ~config_parameters()
    {
    }

    // ---- control params ----
    int irun_; // 0 for initial run, any non-zero value for restart
    int mstep_; // # of time steps
    int msnap_; // # of snapshots
    int ndiag_; // do diag when mod(istep,ndiag)=0
    double nonlinear_; // 1.0: nonlinear run, 0.0: linear run
    double paranl_; // 0: adiabatic electron, >1: kinetic electron
    int mode00_; // 1: keep parallel nonlinearity

    // --- run size (both mtheta and mzetamax should be multiples of # of PEs)
    double tstep_; // 1 include (0,0) mode, 0 exclude (0,0) mode
    int micell_; // time step (unit=L_T/v_th), tstep*\omega_transit<0.1
    int mecell_; // # of ions per grid cell
    int mpsi_; // # of electrons per grid cell
    int mthetamax_; // total # of radial grid points
    int mzetamax_; // poloidal grid, even and factors of 2,3,5 for FFT
    int npartdom_; // number of particle domain partitions per tor dom.
    int ncycle_; // subcycle electron

    // ----  run geometry
    double a_; // minor radius, unit=R_0
    double a0_; // inner boundary, unit=a
    double a1_; // outer boundary, unit=a
    double q0_; // q_profile, q=q0 + q1*r/a + q2 (r/a)^2
    double q1_; //
    double q2_; //
    double rc_; // kappa=exp{-[(r-rc)/rw]**6}
    double rw_; // rc in unit of (a1+a0) and rw in unit of (a1-a0)

    // ----  species information
    double aion_; // species isotope #
    double qion_; // charge state
    double aelectron_; //
    double qelectron_; //

    // ----  equilibrium unit: R_0=1, Omega_c=1, B_0=1, m=1, e=1
    double kappati_; // grad_T/T
    double kappate_; //
    double kappan_; // inverse of eta_i, grad_n/grad_T
    bool fixed_Tprofile; // Maintain Temperature profile (0=no, >0 =yes)
    double tite_; // T_i/T_e
    double flow0_; // d phi/dpsi=gyroradius*[flow0+flow1*r/a+
    double flow1_; //                      flow2*(r/a)**2]
    double flow2_; //

    // ----  physical unit
    double r0_; // major radius (unit=cm)
    double b0_; // on-axis vacuum field (unit=gauss)
    double temperature_; // electron temperature (unit=ev)
    double edensity0_; // electron number density (1/cm^3)

    // -----  standard output: use 0 or 6 to terminal and 11 to file 'stdout.out'
    int nbound_; // 0 for periodic, >0 for zero boundary
    double umax_; // unit=v_th, maximum velocity in each direction
    int iload_; // 0: uniform, 1: non-uniform
    double tauii_; // -1.0: no collisions, 1.0: collisions
    bool track_particles_; // true: keep track of some particles
    int nptrack_; // track nptrack particles every time step
    long rng_control_; // controls seed and algorithm for random num. gen.

    // ---- mode diagnostic: 8 modes.
    std::vector<int> nmode_; // n: toroidal mode number
    std::vector<int> mmode_; // m: poloidal mode number

    int nhybrid_;
    int ntracer_;
    int ntracer1_;
    double gyroradius_;
    int numberpe_;
    int mype_;

    bool do_collision_;
    int mtdiag_;


    int idiag1_;
    int idiag2_;


    int limit_vpara_;

    int mflux_;
    int num_mode_;
    int m_poloidal_;



    int nparam_;


    //these fields change over the course of the run!
    int idiag_;
    int irk_;
    int ihybrid_;
  };

  /**
   * The particle_decomp module from setup.F90
   */
  class particle_decomp : public gtc_module
  {
  protected:
    particle_decomp();

  public:
    typedef sprockit::refcount_ptr<particle_decomp> ptr;

    int ntoroidal_;

    MPI_Comm partd_comm_;
    int nproc_partd_;
    int myrank_partd_;
    MPI_Comm toroidal_comm_;
    int nproc_toroidal_;
    int myrank_toroidal_;
    int left_pe_;
    int right_pe_;
    int toroidal_domain_location_;
    int particle_domain_location_;
    int mzeta_;
    double zetamin_;
    double zetamax_;
    double deltaz_;

  public:
    static ptr
    construct()
    {
      return new particle_decomp();
    }

    virtual
    ~particle_decomp()
    {
    }

    void
    setup(const config_parameters::ptr& params);
  };

  /**
   * The field_array module from module.F90
   */
  class field_array : public gtc_module
  {
  protected:
    field_array(int mmpsi = 192);

  public:
    typedef sprockit::refcount_ptr<field_array> ptr;

    static ptr
    construct(int mpsi)
    {
      return new field_array(mpsi);
    }

    virtual
    ~field_array()
    {
    }

    void
    setup(const config_parameters::ptr& params, double gyroradius,
        const particle_decomp::ptr& partdecomp);

    // --- derived things
    int mimax_;
    int memax_;
    int mgrid_;

    double mi_local_;
    double me_local_;

    int mi_;
    int me_;

    int me1_;

    double deltar_;
    std::vector<int> mtheta_;
  };







} /* namespace sstmac */
#endif /* GTC_MODULES_H_ */