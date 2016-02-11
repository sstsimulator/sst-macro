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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_THERMO_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_THERMO_H_INCLUDED

#include <minimd.h>
#include <minimd-force.h>
#include <minimd-atom.h>

namespace mini {

  /**
   * Pretend to do thermodynamics.
   */
  class minimd::thermo : public sprockit::ptr_type  {

  private:
    lib_compute_minimd* execution_;
    mpi_api* mpi_;

  public:
    typedef sprockit::refcount_ptr<minimd::thermo> ptr;

    virtual std::string
    to_string() const{
      return "thermo";
    }

    int nstat;
    int ntimes;

    void init(lib_compute_minimd* exe,
              mpi_api* mpi)
    {
      execution_ = exe;
      mpi_ = mpi;
    }

    void compute(int iflag, atom::ptr atm,
        const sprockit::refcount_ptr<neighbor>& nbr,
                 force::ptr frc)
    {
      if (iflag > 0 && iflag % nstat) return;
      if (iflag == -1 && nstat > 0 && ntimes % nstat == 0) return;      

      temperature(atm);
      energy(atm);
      frc->compute(atm, nbr);
      pressure(atm);
      execution_->compute("Thermo::compute", NULL, NULL);
    }
    
    void energy(atom::ptr at) {
      execution_->compute("Thermo::energy", at, NULL);
      mpi_->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, mpi_->comm_world());
    }

    void temperature(atom::ptr at) {
      execution_->compute("Thermo::temperature", at, NULL);
      mpi_->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, mpi_->comm_world());
    }

    void pressure(atom::ptr at) {
      execution_->compute("Thermo::pressure", at, NULL);
      mpi_->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, mpi_->comm_world()); 
    }
  };

} 

#endif
