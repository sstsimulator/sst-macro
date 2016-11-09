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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_GTC_GTC_MAIN_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_GTC_GTC_MAIN_H_INCLUDED

#include <sstmac/software/process/app.h>
#include <mpi.h>
#include <cmath>

#include <gtc_modules.h>

#ifdef _USE_LOOPS
#include <sstmac/software/libraries/compute/lib_compute_loops.h>
#endif

#ifdef _USE_EIGER
#include <sstmac/tools/eiger/api/sstmac_compute_eiger.h>
#endif

#ifdef _USE_EIGER
#ifdef _USE_LOOPS
#error "Invalid compute model. #define either _USE_LOOPS or _USE_EIGER."
#endif
#endif

#ifndef _USE_EIGER
#ifndef _USE_LOOPS
#error "Invalid compute model. #define either _USE_LOOPS or _USE_EIGER."
#endif
#endif

#include <sstmac/common/rng.h>

namespace gtc
{

  /**
   * Basic MPI ping-pong.
   */
  class gtc_main :
    public sstmac::sw::app
  {
    std::map<std::string,double> param_map_;

  public:
    gtc_main(sprockit::sim_parameters* params, sstmac::sw::software_id sid,
             sstmac::sw::operating_system* os);

    /// Goodbye.
    virtual
    ~gtc_main() //throw ()
    {
    }

    /// Go.
    void
    skeleton_main() override;

    // -------------- GTC Functions
  private:

    void
    setup();

    void
    rand_num_gen_init();

    void
    load();

    void
    read_input_params();

    void
    chargei(int istep);

    void
    smooth(int something);

    void
    field();

    void
    pushi(int istep);

    void
    diagnosis(int istep);

    void
    shifti();

    void
    collision(int istep, int neop = 32, int neot = 1, int neoz = 1);

    void
    pushe(int s1, int s2, int istep);

    void
    shifte();

    void
    chargee();

    void
    poisson(int istep, int iflag);

    void
    snapshot();

    double sign(double ret, double of){
      return (of >= 0) ? ret : (-1*ret);
    }

    double
    modulo(double num, double by){
      int div = num / by;
      double temp = by * div;
      double ret = std::fabs(num - temp);
      if(sign(1, num) != sign(1, by)){
          ret--;
      }
      return sign(num, by);
    }

    // -------------Configuration Parameters ----------------- //
    // ------------------------------------------------------- //
  private:

    config_parameters::ptr gtcparams_;

    // ------- added by GRH for control ---
    int nthreads_;
    field_array::ptr field_array_;
    particle_decomp::ptr part_decomp_;

    sumi::mpi_api* mpi_;
    sumi::mpi_api* mpi() const {
      return mpi_;
    }


    RNG::SHR3* rng_;

#ifdef _USE_LOOPS
    sstmac::sw::lib_compute_loops* libloops_;
#endif

  };

} // end of namespace sstmac

#endif
