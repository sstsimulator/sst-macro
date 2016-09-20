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
#include <sstmac/common/event_scheduler.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/errors.h>

ImplementFactory(sstmac::stat_collector);

namespace sstmac {

stat_collector::~stat_collector()
{
  // TODO Auto-generated destructor stub
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

stat_collector::stat_collector(sprockit::sim_parameters* params) :
  registered_(false),
  id_(-1),
  params_(params)
{
  fileroot_ = params->get_param("fileroot");
  if (params->has_param("suffix")){
    fileroot_ = fileroot_ + "." + params->get_param("suffix");
  }
  id_ = params->get_optional_int_param("id", -1);
}

stat_collector*
stat_collector::optional_build(sprockit::sim_parameters* params,
                      const std::string& ns,
                      const std::string& deflt,
                      const char* suffix)
{
  if (params->has_namespace(ns)){
    sprockit::sim_parameters* ns_params = params->get_namespace(ns);
    if (suffix){
      ns_params = ns_params->get_optional_namespace(suffix);
      ns_params->add_param_override("suffix", suffix);
    }

    stat_collector* stats = stat_collector_factory::get_optional_param(
          "type", deflt, ns_params);

    return stats;
  } else {
    return nullptr;
  }
}

void
register_optional_stat(event_scheduler *parent, stat_collector *coll)
{
  parent->register_stat(coll);
}



} // end of namespace sstmac

