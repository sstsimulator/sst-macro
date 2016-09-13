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

#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

key::category lib_compute_memmove::key_category("Memory");

lib_compute_memmove::lib_compute_memmove(sprockit::sim_parameters* params,
                                         software_id id, operating_system* os) :
  lib_compute_memmove(params, "libmemmove", id, os)
{
  key_cat_ = key_category;
}

lib_compute_memmove::lib_compute_memmove(sprockit::sim_parameters* params,
                                         const char* prefix, software_id sid,
                                         operating_system* os) :
  lib_compute_inst(params, prefix, sid, os)
{
  key_cat_ = key_category;

  access_width_bytes_ = params->get_optional_int_param("lib_compute_access_width", 64) / 8;
}

void
lib_compute_memmove::do_access(long bytes)
{
  //if (bytes == 0){
  //  return;
  //}
  long num_loops = bytes / access_width_bytes_;
  int nflops = 0;
  int nintops = 1; //memmove instruction
  compute_loop(num_loops, nflops, nintops, access_width_bytes_);
}

void
lib_compute_memmove::read(long bytes)
{
  SSTMACBacktrace("memread");
  do_access(bytes);
}

void
lib_compute_memmove::write(long bytes)
{
  SSTMACBacktrace("memwrite");
  do_access(bytes);
}

void
lib_compute_memmove::copy(long bytes)
{
  SSTMACBacktrace("memcopy");
  do_access(bytes);
}


}
} //end of namespace sstmac

