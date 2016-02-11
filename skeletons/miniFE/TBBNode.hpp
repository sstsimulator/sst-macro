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

#ifndef TBBNODE_HPP_
#define TBBNODE_HPP_

#include <tbb/blocked_range.h>
#include <tbb/parallel_for.h>
#include <tbb/parallel_reduce.h>
#include <tbb/task_scheduler_init.h>
#include <stdlib.h>

#include <NoOpMemoryModel.hpp>

#include <iostream> // debug

template <class WDPin>
struct BlockedRangeWDP {
  mutable WDPin wd;
  BlockedRangeWDP(WDPin &in) : wd(in) {}
  inline void operator()(tbb::blocked_range<int> &rng) const
  {
    for(int i=rng.begin(); i<rng.end(); ++i) {
      wd(i);
    }
  }
};

template <class WDPin>
struct BlockedRangeWDPReducer {
  WDPin wd;
  BlockedRangeWDPReducer(WDPin &in) : wd(in) {}
  BlockedRangeWDPReducer(BlockedRangeWDPReducer &in, tbb::split) : wd(in.wd)
  {
    wd.result = wd.identity();
  }
  void operator()(tbb::blocked_range<int> &rng)
  { 
    for(int i=rng.begin(); i<rng.end(); ++i) {
      wd.result = wd.reduce(wd.result, wd.generate(i));
    }
  }
  inline void join( const BlockedRangeWDPReducer<WDPin> &other ) {
    wd.result = wd.reduce( wd.result, other.wd.result );
  }
};

class TBBNode : public NoOpMemoryModel {
  public:

    TBBNode(int numThreads=0) {
      if (numThreads >= 1) {
        tsi_.initialize(numThreads);
      }
      else {
        tsi_.initialize(tbb::task_scheduler_init::automatic);
      }
    }

    ~TBBNode() {}

    template <class WDP>
    void parallel_for(int length, WDP wd) {
      BlockedRangeWDP<WDP> tbb_wd(wd);
      tbb::parallel_for(tbb::blocked_range<int>(0,length), tbb_wd, tbb::auto_partitioner()); 
    }

    template <class WDP>
    void parallel_reduce(int length, WDP &wd) {
      BlockedRangeWDPReducer<WDP> tbb_wd(wd);
      tbb::parallel_reduce(tbb::blocked_range<int>(0,length), tbb_wd, tbb::auto_partitioner());
      wd.result = tbb_wd.wd.result;  // have to put result from final tbb_wd into orginal wd
    }

  private:
    static tbb::task_scheduler_init tsi_;
};

#endif
