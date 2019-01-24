/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_LIBRARY_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_LIBRARY_H_INCLUDED

#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/event_location.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/libraries/library_fwd.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/factories/factory.h>
#include <map>


namespace sstmac {
namespace sw {

class Library
{
  DeclareFactory(Library, SoftwareId, OperatingSystem*)
 public:
  std::string toString() const {
    return libname_;
  }

  const std::string& libName() const {
    return libname_;
  }

  virtual void incomingEvent(Event* ev) = 0;

  OperatingSystem* os() const {
    return os_;
  }

  SoftwareId sid() const {
    return sid_;
  }

  int aid() const {
    return sid_.app_;
  }

  NodeId addr() const {
    return addr_;
  }

  virtual ~Library();

 protected:
  Library(const std::string& libname, SoftwareId sid, OperatingSystem* os);

  Library(const char* prefix, SoftwareId sid, OperatingSystem* os) :
    Library(standardLibname(prefix, sid), sid, os)
  {
  }

  static std::string standardLibname(const char* prefix, SoftwareId sid){
    return standardLibname(prefix, sid.app_, sid.task_);
  }

  static std::string standardLibname(const char* prefix, AppId aid, TaskId tid){
    std::string app_prefix = standardAppPrefix(prefix, aid);
    return standardAppLibname(app_prefix.c_str(), tid);
  }

  static std::string standardAppLibname(const char* prefix, TaskId tid){
    return sprockit::printf("%s-%d", prefix, tid);
  }

  static std::string standardAppPrefix(const char* prefix, AppId aid){
    return sprockit::printf("%s-%d", prefix, aid);
  }

 protected:
  OperatingSystem* os_;
  SoftwareId sid_;
  NodeId addr_;

 private:
  std::string libname_;

};

}
} //end of namespace sstmac

#endif
