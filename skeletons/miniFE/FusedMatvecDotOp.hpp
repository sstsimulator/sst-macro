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

#ifndef FUSEDMATVECDOTOP_HPP_
#define FUSEDMATVECDOTOP_HPP_

#ifndef KERNEL_PREFIX
#define KERNEL_PREFIX
#endif

template <typename MatrixType,
          typename VectorType>
struct FusedMatvecDotOp {

  typedef typename VectorType::GlobalOrdinalType GlobalOrdinalType;
  typedef typename VectorType::LocalOrdinalType LocalOrdinalType;
  typedef typename VectorType::ScalarType ScalarType;
  typedef ScalarType ReductionType;

  size_t n;

  const LocalOrdinalType*  Arowoffsets;
  const GlobalOrdinalType* Acols;
  const ScalarType*        Acoefs;

  const ScalarType* x;
        ScalarType* y;
  ScalarType beta;

  ReductionType result;

  inline FusedMatvecDotOp() {
    result = identity();
  }

  static inline KERNEL_PREFIX ReductionType identity()
  {
    return 0.0;
  }

  inline KERNEL_PREFIX ReductionType reduce(ReductionType u, ReductionType v) const
  {
    return u+v;
  }

  inline KERNEL_PREFIX ScalarType generate(int row)
  {
    //we count on the caller (ComputeNode) to pass in 'row'
    //in range 0..n-1
  
    ScalarType sum = beta*y[row];

    for(LocalOrdinalType i=Arowoffsets[row]; i<Arowoffsets[row+1]; ++i) {
      sum += Acoefs[i]*x[Acols[i]];
    }

    y[row] = sum;
    return x[row]*sum;
  }
};

#endif
