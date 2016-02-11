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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_INTEGRATE_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_INTEGRATE_H_INCLUDED

#include <minimd.h>
#include <minimd-comm.h>
#include <minimd-timer.h>
#include <minimd-force.h>
#include <minimd-thermo.h>
#include <minimd-neighbor.h>

namespace mini {

  using namespace boost;

  /**
   * Take over the roles of the time integrator in miniMD.
   */
  class minimd::integrate : public sprockit::ptr_type  {
  private:
    lib_compute_minimd* execution_;
    mpi_api* mpi_;

  public:
    typedef sprockit::refcount_ptr<minimd::integrate> ptr;

    virtual std::string 
    to_string() const{
        return "integrate";
    }

    double dt;
    int ntimes;
    
    void init(lib_compute_minimd* exe,
              mpi_api* mpi)
    {
      execution_ = exe;
      mpi_ = mpi;
    }

    void run(const sprockit::refcount_ptr<atom>& atm, const sprockit::refcount_ptr<force>& frc,
             const sprockit::refcount_ptr<neighbor>& nbr, const sprockit::refcount_ptr<comm>& cmm,
             const sprockit::refcount_ptr<thermo>& thm, const sprockit::refcount_ptr<timer>& tmr)
    {
      mpi_id rank = mpi_->comm_world()->rank();
//#define SSTMAC_DB(command) std::cerr << rank << " " << #command << "\n"; command;
#define SSTMAC_DB(command) command
      for(int n = 0; n < ntimes; ++n) {
        SSTMAC_DB( execution_->compute("Integrate::run", atm, NULL, 1) );

        if((n+1) % nbr->every) {
          SSTMAC_DB( cmm->communicate(atm) );
          SSTMAC_DB( tmr->stamp(timer::TIME_COMM) );
        }
        else {
          SSTMAC_DB( cmm->exchange(atm) );
          SSTMAC_DB( cmm->borders(atm) );
          SSTMAC_DB( tmr->stamp(timer::TIME_COMM) );
          SSTMAC_DB( nbr->build(atm) );
          SSTMAC_DB( tmr->stamp(timer::TIME_NEIGH) );
        }
        
        SSTMAC_DB( frc->compute(atm, nbr) );
        SSTMAC_DB( tmr->stamp(timer::TIME_FORCE) );

        SSTMAC_DB( cmm->reverse_communicate(atm) );
        SSTMAC_DB( tmr->stamp(timer::TIME_COMM) );

        SSTMAC_DB( execution_->compute("Integrate::run", atm, NULL, 2) );
        if(thm->nstat) {
          SSTMAC_DB( thm->compute(n+1, atm, nbr, frc) );
        }
      }
#undef SSTMAC_DB
    }
  };

} 

#endif
