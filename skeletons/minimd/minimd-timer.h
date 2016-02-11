/*
 *  This file is part of SST/macroscale: 
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top 
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_TIMER_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_TIMER_H_INCLUDED

#include <minimd.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/common/sstmac_config.h>

namespace mini {

  /**
   * A stand-in for the miniMD timer functions.
   */
  class minimd::timer : public sprockit::ptr_type  {
    mpi_api* mpi_;

  public:
    typedef sprockit::refcount_ptr<minimd::timer> ptr;

    virtual std::string
    to_string() const{
      return "timer";
    }

    enum ttag {
      TIME_TOTAL=0, TIME_COMM, TIME_FORCE, TIME_NEIGH, TIME_N
    };

    timestamp previous_time;
    std::vector<timestamp> array;

    void init(lib_compute_minimd*, mpi_api* mpi)
    {
      mpi_ = mpi;
    }
    
    void stamp() {
      previous_time = mpi_->wtime();
    }
    
    void stamp(ttag tag) {
      if(array.size() < TIME_N) array.resize(TIME_N);
      array.at(tag) += mpi_->wtime() - previous_time;
    }

    void barrier_start(ttag tag) {
      if(array.size() < TIME_N) array.resize(TIME_N);
      mpi_->barrier(mpi_->comm_world());
      array.at(tag) = mpi_->wtime();
    }

    void barrier_stop(ttag tag) {
      if(array.size() < TIME_N) array.resize(TIME_N);
      mpi_->barrier(mpi_->comm_world());
      array.at(tag) = mpi_->wtime() - array.at(tag);
    }
  };

} 

#endif
