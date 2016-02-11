/*
//@HEADER
// ************************************************************************
// 
//               HPCCG: Simple Conjugate Gradient Benchmark Code
//                 Copyright (2006) Sandia Corporation
// 
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
// 
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//  
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//  
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER
*/

#ifndef DOTOP_HPP_
#define DOTOP_HPP_

template <class Scalar>
struct DotOp {
  typedef Scalar ReductionType;

  const Scalar* x;
  const Scalar* y;

  size_t n;

  ReductionType result;

  inline DotOp() {
    result = identity();
  }

  static inline KERNEL_PREFIX ReductionType identity()
  {
    return 0.0;
  }

  inline KERNEL_PREFIX ReductionType reduce(ReductionType u, ReductionType v) const
  {
#ifndef _USE_LOOP_MODEL
    return u+v;
#else
    return 1;
#endif
  }

  inline KERNEL_PREFIX Scalar generate(int i) const
  {
#ifndef _USE_LOOP_MODEL
    return x[i]*y[i];
#else
    return 1;
#endif
  }
};

#endif
