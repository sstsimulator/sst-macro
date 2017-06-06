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

namespace gtc {

/**
 * Basic MPI ping-pong.
 */
class gtc_main :
  public sstmac::sw::app
{
  FactoryRegister("gtc", sstmac::sw::app, gtc_main, "gyroknetic toroidal code")
 private:
  std::map<std::string,double> param_map_;

 public:
  gtc_main(sprockit::sim_parameters* params, sstmac::sw::software_id sid,
           sstmac::sw::operating_system* os);

  /// Goodbye.
  virtual ~gtc_main() {}

  /// Go.
  void skeleton_main() override;

  // -------------- GTC Functions
 private:
  void setup();

  void rand_num_gen_init();

  void load();

  void read_input_params();

  void chargei(int istep);

  void smooth(int something);

  void field();

  void pushi(int istep);

  void diagnosis(int istep);

  void shifti();

  void collision(int istep, int neop = 32, int neot = 1, int neoz = 1);

  void pushe(int s1, int s2, int istep);

  void shifte();

  void chargee();

  void poisson(int istep, int iflag);

  void snapshot();

  double sign(double ret, double of){
    return (of >= 0) ? ret : (-1*ret);
  }

  double modulo(double num, double by){
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