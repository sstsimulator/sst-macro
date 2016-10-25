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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_LIBRARY_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_LIBRARY_H_INCLUDED

#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/event_location.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/libraries/library_fwd.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/factories/factory.h>
#include <map>


namespace sstmac {
namespace sw {

class library {
  
 public:
  std::string
  to_string() const {
    return libname_;
  }

  virtual std::string
  lib_name() const {
    return libname_;
  }

  virtual void
  incoming_event(event* ev) = 0;

  operating_system*
  os() const {
    return os_;
  }

  software_id
  sid() const {
    return sid_;
  }

  int
  aid() const {
    return sid_.app_;
  }

  node_id
  addr() const {
    return addr_;
  }

  device_id
  event_location() const;

  virtual ~library();

 protected:
  library(const std::string& libname, software_id sid, operating_system* os);

  library(const char* prefix, software_id sid, operating_system* os) :
    library(standard_lib_name(prefix, sid), sid, os)
  {
  }

  static std::string
  standard_lib_name(const char* prefix, software_id sid){
    return standard_lib_name(prefix, sid.app_, sid.task_);
  }

  static std::string
  standard_lib_name(const char* prefix, app_id aid, task_id tid){
    std::string app_prefix = standard_app_prefix(prefix, aid);
    return standard_app_lib_name(app_prefix.c_str(), tid);
  }

  static std::string
  standard_app_lib_name(const char* prefix, task_id tid){
    return sprockit::printf("%s-%d", prefix, tid);
  }

  static std::string
  standard_app_prefix(const char* prefix, app_id aid){
    return sprockit::printf("%s-%d", prefix, aid);
  }

 protected:
  operating_system* os_;
  key::category key_cat_;
  software_id sid_;
  node_id addr_;

 private:
  std::string libname_;

};

class blocking_library :
  public library
{
 protected:
  blocking_library(const char* prefix, software_id sid, operating_system* os) :
    library(prefix, sid, os)
  {
  }

  blocking_library(const std::string& libname, software_id sid, operating_system* os) :
    library(libname, sid, os)
  {
  }

  void wait_event(event* ev, key::category = key::general);

  virtual void
  incoming_event(event *ev);

 private:
  std::map<event*,key*> blocked_events_;

};

DeclareFactory(library, software_id, operating_system*);

}
} //end of namespace sstmac

#endif

