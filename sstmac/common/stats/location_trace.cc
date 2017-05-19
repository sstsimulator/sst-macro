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

#include <sstmac/common/stats/location_trace.h>
#include <sprockit/util.h>

namespace sstmac {

#define cast_bytes(x)  reinterpret_cast<char*>(&x)

void
location_trace::collect(
  timestamp created,
  device_id creator,
  timestamp scheduled,
  device_id runner)
{
  event ev;
  ev.created = created;
  ev.creator = creator;
  ev.scheduled = scheduled;
  ev.runner = runner;
  local_events_.push_back(ev);
}

void
location_trace::clear()
{
  local_events_.clear();
}

void
location_trace::reduce(stat_collector *coll)
{
  location_trace* tr = safe_cast(location_trace, coll);
  std::list<event>::iterator it, end = tr->local_events_.end();
  for (it=tr->local_events_.begin(); it != end; ++it){
    event& ev = *it;
    global_events_[ev.scheduled] = ev;
  }
}

void
location_trace::global_reduce(parallel_runtime *rt)
{
  spkt_throw(sprockit::unimplemented_error,
    "location_trace::global_reduce: location trace should not be run in parallel");
}

void
location_trace::dump_global_data()
{
  std::string fname = sprockit::printf("%s.events.bin", fileroot_.c_str());
  std::fstream myfile;
  stat_collector::check_open(myfile, fname, std::ios::out | std::ios::binary);

  std::map<timestamp, event>::iterator it, end = global_events_.end();
  for (it=global_events_.begin(); it != end; ++it){
    event& ev = it->second;
    myfile.write(cast_bytes(ev), sizeof(event));
  }
  myfile.close();
}

void
location_trace::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d.events.bin", fileroot_.c_str(), id_);
  std::fstream myfile;
  stat_collector::check_open(myfile, fname, std::ios::out | std::ios::binary);

  std::list<event>::iterator it, end = local_events_.end();
  for (it=local_events_.begin(); it != end; ++it){
    event& ev = *it;
    myfile.write(cast_bytes(ev), sizeof(event));
  }
  myfile.close();
}

bool
location_trace::read(
  std::istream& myfile,
  timestamp &created,
  device_id &creator,
  timestamp &scheduled,
  device_id &runner)
{
  if (myfile.eof()) {
    return false;
  }

  event ev;
  myfile.read(cast_bytes(ev), sizeof(event));
  created = ev.created;
  creator = ev.creator;
  scheduled = ev.scheduled;
  runner = ev.runner;
  return true;
}

}