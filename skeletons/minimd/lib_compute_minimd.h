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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_LIB_COMPUTE_MINIMD_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_LIB_COMPUTE_MINIMD_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_time.h>
#include <sstmac/sstmacro.h>
#include <sstmac/software/libraries/compute/lib_compute_loops.h>

namespace mini
{

  class lib_compute_minimd : public sstmac::sw::lib_compute_time
  {
  public:
    virtual
    ~lib_compute_minimd();

    lib_compute_minimd(software_id id);

    // --------------------------------------//

  public:
    void
    compute(std::string func, const sprockit::refcount_ptr<sprockit::ptr_type> &a1,
        const sprockit::refcount_ptr<sprockit::ptr_type> &a2, int index = 0);

    void
    compute(timestamp t)
    {
      lib_compute_time::compute(t);
    }

    int
    get_int(const std::string& func, int index = 0);

  protected:
    sstmac::sw::software_id id_;
    std::map<std::string, timestamp> funcs_;
    bool using_loops_;
    sstmac::sw::lib_compute_loops* libloop_;
  };

}
#endif
