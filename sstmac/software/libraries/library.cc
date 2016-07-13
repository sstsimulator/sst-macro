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

#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/sstmac_env.h>

namespace sstmac {
namespace sw {

static std::map<std::string, library_builder*>* library_builders_ = 0;

void
library::register_builder(const std::string &libname,
                          library_builder *builder)
{
  if (!library_builders_) {
    library_builders_ = new std::map<std::string, library_builder*>;
  }
  (*library_builders_)[libname] = builder;
}

library_builder*
library::builder(const std::string &libname)
{
  if (!library_builders_) {
    spkt_throw_printf(sprockit::value_error, "no library builders registered");
  }

  library_builder* builder = (*library_builders_)[libname];
  if (!builder) {
    spkt_throw_printf(sprockit::value_error,
        "name %s is not known for constructing library",
        libname.c_str());
  }
  return builder;
}

library*
library::construct_lib(const std::string& libname, const software_id& sid)
{
  library_builder* blder = builder(libname);
  std::string sid_libname = libname + sid.to_string();
  library* lib = blder->construct(sid_libname);
  return lib;
}

library*
library::construct_lib(const std::string& libname)
{
  library_builder* bldr = builder(libname);
  library* lib = bldr->construct(libname);
  return lib;
}

library::library() :
  os_(0)
{
}

library::~library()
{
}

void
library::unregister_all_libs()
{
  os_->unregister_all_libs(this);
}

void
library::register_lib(library* lib)
{
#if SSTMAC_SANITY_CHECK
  if (!os_){
    spkt_throw(sprockit::value_error,
        "library::register_lib: os not initialized yet")
  }
#endif
  os_->register_lib(this, lib);
}

void
library::init_os(operating_system* os)
{
  os_ = os;

#if SSTMAC_SANITY_CHECK
  if (!os->params()){
    spkt_throw(sprockit::null_error, "operating_system::params is null");
  }
#endif
  consume_params(os->params());
}

void
library::consume_params(sprockit::sim_parameters* params)
{
  //do nothing by default
}

void
library::incoming_event(event* ev)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "%s::incoming_event: this library should only block, never receive incoming",
     to_string().c_str());
}


}
} //end namespace


