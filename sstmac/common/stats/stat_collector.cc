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

#include <sstmac/common/stats/stat_collector.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/errors.h>

ImplementFactory(sstmac::stat_collector);

namespace sstmac {

stat_collector::~stat_collector()
{
  // TODO Auto-generated destructor stub
}

void
stat_collector::add_suffix(const std::string& suffix)
{
  fileroot_ = fileroot_ + "." + suffix;
}

bool
stat_collector::check_open(std::fstream& myfile, const std::string& fname, std::ios::openmode ios_flags)
{
  if (!myfile.is_open()) {
    myfile.open(fname.c_str(), ios_flags);
    if (!myfile.is_open()) {
      spkt_throw_printf(sprockit::io_error,
                       "stat_collector: cannot open file %s for writing",
                       fname.c_str());
    }
  }
  return true;
}

void
stat_collector::init_factory_params(sprockit::sim_parameters* params)
{
  fileroot_ = params->get_param("fileroot");
  id_ = params->get_optional_int_param("id", -1);
}

void
stat_collector::clone_into(stat_collector *cln) const
{
  cln->fileroot_ = fileroot_;
}

} // end of namespace sstmac

