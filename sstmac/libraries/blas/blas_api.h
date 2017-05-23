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

#ifndef sstmac_software_libraries_socketapiS_H
#define sstmac_software_libraries_socketapiS_H

#include <sstmac/software/api/api.h>
#include <sstmac/software/libraries/compute/lib_compute_fwd.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/operating_system_fwd.h>

DeclareDebugSlot(blas);

namespace sstmac {
namespace sw {

class blas_kernel
{
  DeclareFactory(blas_kernel)
 public:
  virtual std::string to_string() const = 0;

  virtual compute_event* op_3d(int m, int k, int n);

  virtual compute_event* op_2d(int m, int n);

  virtual compute_event* op_1d(int n);

};

class blas_api :
  public api
{
  RegisterAPI("blas", blas_api)

 public:
  blas_api(sprockit::sim_parameters* params,
           software_id sid,
           operating_system* os);

  virtual ~blas_api();

  /**
   A(m,n) * B(n,k) = C(m,k)
  */
  void dgemm(int m, int n, int k);

  /**
   A(m,n) * X(n) = B(m)
  */
  void dgemv(int m, int n);

  void daxpy(int n);

  void ddot(int n);

  void incoming_event(event *ev){
    library::incoming_event(ev);
  }

 protected:
  void init_kernels(sprockit::sim_parameters* params);

 protected:
  lib_compute_inst* lib_compute_;

  software_id id_;

  static blas_kernel* ddot_kernel_;
  static blas_kernel* dgemm_kernel_;
  static blas_kernel* dgemv_kernel_;
  static blas_kernel* daxpy_kernel_;

};


}
} //end of namespace sstmac

#endif // socketapiS_H