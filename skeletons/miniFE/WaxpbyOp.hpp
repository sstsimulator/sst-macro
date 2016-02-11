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

#ifndef WAXPBYOP_HPP_
#define WAXPBYOP_HPP_

#ifndef KERNEL_PREFIX 
#define KERNEL_PREFIX
#endif

template <class Scalar>
struct WaxpbyOp {
      Scalar* w;
  const Scalar* x;
  const Scalar* y;
  Scalar alpha, beta;
  size_t n;
  KERNEL_PREFIX void operator()(size_t i) const
  {
    //here we count on the caller (ComputeNode) to pass in 'i'
    //that is in the range 0..n-1
    w[i] = alpha*x[i] + beta*y[i];
  }
};

template <class Scalar>
struct FusedWaxpbyOp {
      Scalar* w;
  const Scalar* x;
  const Scalar* y;
  Scalar alpha, beta;
      Scalar* w2;
  const Scalar* x2;
  const Scalar* y2;
  Scalar alpha2, beta2;
  size_t n;
  KERNEL_PREFIX void operator()(size_t i) const
  {
    //here we count on the caller (ComputeNode) to pass in 'i'
    //that is in the range 0..n-1
    w[i] = alpha*x[i] + beta*y[i];
    w2[i] = alpha2*x2[i] + beta2*y2[i];
  }
};

#endif
