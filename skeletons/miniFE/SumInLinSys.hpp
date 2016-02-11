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

#ifndef _SUMINLINSYS_HPP_
#define _SUMINLINSYS_HPP_

#include <Hex8_enums.hpp>
#include <LockingMatrix.hpp>
#include <LockingVector.hpp>

template<typename GlobalOrdinal,typename Scalar,
         typename MatrixType, typename VectorType>
struct SumInLinSys {
  GlobalOrdinal* node_ordinals;
  Scalar* elem_diffusion_matrix;
  Scalar* elem_source_vector;
  miniFE::LockingMatrix<MatrixType>* A;
  miniFE::LockingVector<VectorType>* b;

inline void operator()(int i)
{
  size_t nnodes = miniFE::Hex8::numNodesPerElem;
  GlobalOrdinal* node_ords = node_ordinals+i*nnodes;
  Scalar* diffusionMat = elem_diffusion_matrix+i*nnodes*nnodes;
  Scalar* sourceVec = elem_source_vector+i*nnodes;
  for(size_t ii=0; ii<nnodes; ++ii) {
    GlobalOrdinal row = node_ords[ii];
    A->sum_in(row, nnodes, node_ords,
              &(diffusionMat[ii*nnodes]));
    b->sum_in(1, &row, &(sourceVec[ii]));
  }
}

};

#endif
