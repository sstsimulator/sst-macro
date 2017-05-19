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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_LOOPS_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_LOOPS_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>

namespace sstmac {
namespace sw {

class lib_compute_loops :
  public lib_compute_memmove
{
  // ------- constructor / boost stuff -------------//

 public:
  virtual
  ~lib_compute_loops() {}

  lib_compute_loops(sprockit::sim_parameters* params, software_id id,
                    operating_system* os);

  template<typename Type>
  void
  compute_array_init(long arraysize) {
    doing_memory_ = true;
    lib_compute_memmove::read(arraysize * sizeof(Type));
    doing_memory_ = false;
  }

  /**
   * compute_loop - compute and schedule a for loop's costs as
   * moving Nm bytes and doing Nf flops, where
   * Nm = loop_trips*numlines*mem_line_ratio*mem_op_size
   * Nf = loop_trips*numlines*flop_line_ratio
   * where mem_line_ratio and flop_line_ratio may appear as
   * global simulation parameters or will default (0.8, 0.8) and
   * mem_op_size is currently fixed internally at 16.
   * loop_trips = Product-N(toI-fromI+1 over all N range pairs I in the argument list)
   * Given pipeline, vectorization, and other execution features,
   * numlines is essentially a fudge factor that must be determined
   * by experimental comparison; reasonable initial guesses for the numlines are
   * (a) the number of executable statements in the loop
   * (b) the number of values read from or written to main data memory at the
   * innermost loop level.
   * For all versions of this function, loop_trips is product of the
   * (to-from+1) differences in the argument list.
   * @param from loop counter range begin
   * @param to loop counter range end (assuming step size 1)
   * @param numlines number of lines equivalent of code (>0) tuning parameter
   */
  template<typename T>
  void
  compute_loop(long long from1, long long to1, T numlines) {
    if (do_loops_) {
      long long loop = to1 - from1 + 1; //+1 for fortran loops

      compute_loop_work(loop, (double) numlines);
    }
  }

  /**
   * compute_loop2 - compute for one loop nested in an outer one
   * @param from1
   * @param to1
   * @param from2
   * @param to2
   * @param numlines
   */
  template<typename T>
  void
  compute_loop2(long long from1, long long to1, long long from2,
                long long to2, T numlines) {
    if (do_loops_) {
      long long loop1 = to1 - from1 + 1; //+1 for fortran loops
      long long loop2 = to2 - from2 + 1;
      long long loop = loop1 * loop2;

      compute_loop_work(loop, (double) numlines);
    }
  }

  /**
   * compute_loop3 - compute for one loop nested in loop nested in an outer one
   * @param from1
   * @param to1
   * @param from2
   * @param to2
   * @param from3
   * @param to3
   * @param numlines
   */
  template<typename T>
  void
  compute_loop3(long long from1, long long to1, long long from2,
                long long to2, long long from3, long long to3, T numlines) {
    if (do_loops_) {
      long long loop1 = to1 - from1 + 1; //+1 for fortran loops
      long long loop2 = to2 - from2 + 1;
      long long loop3 = to3 - from3 + 1;
      long long loop = loop1 * loop2 * loop3;

      compute_loop_work(loop, (double) numlines);
    }
  }

  /**
   * compute_loop4 - compute for quadruple nested loop

   * @param numlines
   */
  template<typename T>
  void
  compute_loop4(long long from1, long long to1, long long from2,
                long long to2, long long from3, long long to3, long long from4,
                long long to4, T numlines) {
    if (do_loops_) {
      long long loop1 = to1 - from1 + 1; //+1 for fortran loops
      long long loop2 = to2 - from2 + 1;
      long long loop3 = to3 - from3 + 1;
      long long loop4 = to4 - from4 + 1;
      long long loop = loop1 * loop2 * loop3 * loop4;
      ;

      compute_loop_work(loop, (double) numlines);
    }
  }

  /**
   * compute_loop5 - compute for 5-nested loop
   * @param from1 see compute_loop for parameters explanation.
   * @param to1
   * @param from2
   * @param to2
   * @param from3
   * @param to3
   * @param from4
   * @param to4
   * @param from5
   * @param to5
   * @param numlines
   */
  template<typename T>
  void
  compute_loop5(long long from1, long long to1, long long from2,
                long long to2, long long from3, long long to3, long long from4,
                long long to4, long long from5, long long to5, T numlines) {
    if (do_loops_) {
      long long loop1 = to1 - from1 + 1; //+1 for fortran loops
      long long loop2 = to2 - from2 + 1;
      long long loop3 = to3 - from3 + 1;
      long long loop4 = to4 - from4 + 1;
      long long loop5 = to5 - from5 + 1;
      long long loop = loop1 * loop2 * loop3 * loop4 * loop5;

      compute_loop_work(loop, (double) numlines);
    }
  }

  /**
   * compute_loop6 - compute for 6-nested loop
   * @param from1 see compute_loop for parameters explanation.
   * @param to1
   * @param from2
   * @param to2
   * @param from3
   * @param to3
   * @param from4
   * @param to4
   * @param from5
   * @param to5
   * @param from6
   * @param to6
   * @param numlines
   */
  template<typename T>
  void
  compute_loop6(long long from1, long long to1, long long from2,
                long long to2, long long from3, long long to3, long long from4,
                long long to4, long long from5, long long to5, long long from6,
                long long to6, T numlines) {
    if (do_loops_) {
      long long loop1 = to1 - from1 + 1; //+1 for fortran loops
      long long loop2 = to2 - from2 + 1;
      long long loop3 = to3 - from3 + 1;
      long long loop4 = to4 - from4 + 1;
      long long loop5 = to5 - from5 + 1;
      long long loop6 = to6 - from6 + 1;
      long loop = loop1 * loop2 * loop3 * loop4 * loop5 * loop6;

      compute_loop_work(loop, (double) numlines);
    }
  }

  /**
   * compute_loop6 - compute for 6-nested loop
   * @param from1 see compute_loop for parameters explanation.
   * @param to1
   * @param from2
   * @param to2
   * @param from3
   * @param to3
   * @param from4
   * @param to4
   * @param from5
   * @param to5
   * @param from6
   * @param to6
   * @param from7
   * @param to7
   * @param numlines
   */
  template<typename T>
  void
  compute_loop7(long long from1, long long to1, long long from2,
                long long to2, long long from3, long long to3, long long from4,
                long long to4, long long from5, long long to5, long long from6,
                long long to6, long long from7, long long to7, T numlines) {
    if (do_loops_) {
      long long loop1 = to1 - from1 + 1; //+1 for fortran loops
      long long loop2 = to2 - from2 + 1;
      long long loop3 = to3 - from3 + 1;
      long long loop4 = to4 - from4 + 1;
      long long loop5 = to5 - from5 + 1;
      long long loop6 = to6 - from6 + 1;
      long long loop7 = to7 - from7 + 1;
      long long loop = loop1 * loop2 * loop3 * loop4 * loop5 * loop6
                       * loop7;

      compute_loop_work(loop, (double) numlines);
    }
  }

  void
  compute_fft();

  static int mem_op_size_;
  static double mem_line_ratio_;
  static double flop_line_ratio_;
  static bool do_loops_;

 protected:
  void
  compute_loop_work(long long loop, double numlines);

 protected:
  bool doing_memory_;

};

} //end of namespace sstmac
}

#endif