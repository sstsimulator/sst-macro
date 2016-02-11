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

#ifndef CUDANODE_HPP_
#define CUDANODE_HPP_

#include <CudaMemoryModel.hpp>

// forward declaration
class CUDANode;

class CUDANode : public CudaMemoryModel {
  public:

    CUDANode(int device = 0, int numBlocks = -1, int numThreads = 256, int verbose = 1);

    ~CUDANode();

    //@{ Computational methods

    template <class WDP>
    void parallel_for(int length, WDP wdp);

    template <class WDP>
    void parallel_reduce(int length, WDP& wd);

    //@} 

    static CUDANode& singleton(int device=0, int numBlocks=-1, int numThreads=256)
    {
      static CUDANode* cuda_node = NULL;
      if (cuda_node == NULL) {
        cuda_node = new CUDANode(device, numBlocks, numThreads);
      }
      return *cuda_node;
    }
      
  private:
    //template <class WDP, int FirstLevel>
    //void call_reduce(int length, WDP wd, int threads, int blocks, void * d_blkpart);
    // numBlocks_ is 
    // - the number of blocks launched in a call to parallel_for()
    // - not used by parallel_reduce()
    int numBlocks_;
    // numThreads_ is required to be a power-of-two (our requirement) between 1 and 512 (CUDA's requirement). It is:
    // - the maximum number of threads used by parallel_reduce()
    // - the number of threads per block in a call to parallel_for()
    int numThreads_;
    // total global device memory, in bytes
    int totalMem_;

    void expand_blk_mem(size_t size_in_bytes);

    char* h_blk_mem_;
    void* d_blk_mem_;
    size_t blk_mem_size_;

};

#endif
