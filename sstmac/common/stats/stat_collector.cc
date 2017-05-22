/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/event_scheduler.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/errors.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"suffix",
"fileroot",
);

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
stat_collector::required_build(sprockit::sim_parameters* params,
                      const std::string& ns,
                      const std::string& deflt,
                      const char* suffix)
{
  stat_collector* coll = optional_build(params, ns, deflt, suffix);
  if (!coll){
    stats_error(params, ns, deflt);
  }
  return coll;
}

void
stat_collector::stats_error(sprockit::sim_parameters *params,
                            const std::string &ns,
                            const std::string &deflt)
{
  if (!params->has_namespace(ns)){
    spkt_abort_printf("Could not locate stats namespace %s", ns.c_str());
  } else if (params->has_param("type")){
    const char* ns_str = ns.size() ?  " in namespace " : "";
    spkt_abort_printf("Received invalid stats type %s%s%s- "
                      " a valid value would have been %s",
                      params->get_param("type").c_str(),
                      ns_str, ns.c_str());
  } else {
    spkt_abort_printf("Received invalid stats type %s",
                      deflt.c_str());
  }
}

stat_collector*
stat_collector::optional_build(sprockit::sim_parameters* params,
                      const std::string& ns,
                      const std::string& deflt,
                      const char* suffix)
{
  if (ns.size()){
    if (params->has_namespace(ns)){
      params = params->get_namespace(ns);
    } else {
      return nullptr;
    }
  }

  if (suffix){
    params = params->get_optional_namespace(suffix);
    params->add_param_override("suffix", suffix);
  }

  stat_collector* stats = stat_collector::factory::get_optional_param(
        "type", deflt, params);

  return stats;
}

void
stat_collector::register_optional_stat(event_scheduler* parent, stat_collector *coll, stat_descr_t* descr)
{
  parent->register_stat(coll, descr);
}

stat_value_base::stat_value_base(sprockit::sim_parameters *params) :
  stat_collector(params)
{
  id_ = params->get_int_param("id");
}



} // end of namespace sstmac