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

#ifndef SERIALCOMPUTENODE_HPP_
#define SERIALCOMPUTENODE_HPP_

#include <NoOpMemoryModel.hpp>

class SerialComputeNode : public NoOpMemoryModel {
  public:
    template <class WDP>
    void parallel_for(unsigned int length, WDP wd) {
      for(int i=0; i<length; ++i) {
        wd(i);
      }
    }

    template <class WDP>
    void parallel_reduce(unsigned int length, WDP &wd) {
      wd.result = wd.identity();
      for(int i=0; i<length; ++i) {
        wd.result = wd.reduce(wd.result, wd.generate(i));
      }
    }

};

#endif
