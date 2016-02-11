#ifndef LIB_HYBRID_COMPUTE_H
#define LIB_HYBRID_COMPUTE_H
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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_HYBRID_COMPUTE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_HYBRID_COMPUTE_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>

namespace sstmac {
namespace sw {

class lib_hybrid_compute :
  public lib_compute_inst,
  public lib_compute_memmove
{

 public:

  virtual
  ~lib_hybrid_compute {
  }

 protected:
  lib_hybrid_compute(software_id id);

  lib_hybrid_compute(const std::string& id);

};

} //end of namespace sstmac
}

#endif



#endif // LIB_HYBRID_COMPUTE_H

