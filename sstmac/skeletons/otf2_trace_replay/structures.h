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

#ifndef sstmac_skeletons_otf2_STRUCTURES_H_
#define sstmac_skeletons_otf2_STRUCTURES_H_

#include <string>
#include <unordered_map>
#include <otf2/otf2.h>
#include <sstmac/skeletons/otf2_trace_replay/callid.h>
#include <sprockit/errors.h>

/*
 * Forward declarations for definition tables constructed from definition
 * reader callbacks
 */

struct OTF2_ClockProperties {
  uint64_t timerResolution;
  uint64_t globalOffset;
  uint64_t traceLength;
};

struct OTF2_Location {
  OTF2_StringRef name;
  OTF2_LocationType locationType;
  uint64_t numberOfEvents;
  OTF2_LocationGroupRef locationGroup;
};

struct OTF2_Region {
  OTF2_StringRef name;
  OTF2_StringRef canonicalName;
  OTF2_StringRef description;
  OTF2_RegionRole regionRole;
  OTF2_Paradigm paradigm;
  OTF2_StringRef sourceFile;
};

struct OTF2_Callpath {
  OTF2_CallpathRef parent;
  OTF2_RegionRef region;
};

struct OTF2_Group {
  OTF2_StringRef name;
  OTF2_GroupType groupType;
  OTF2_Paradigm paradigm;
  OTF2_GroupFlag groupFlags;
};

struct OTF2_Comm {
  OTF2_StringRef name;
  OTF2_GroupRef group;
  OTF2_CommRef parent;
};

struct MPINameIDMap {
  std::unordered_map<std::string, MPI_CALL_ID> idMap;
  MPI_CALL_ID get(const std::string& str){
    auto iter = idMap.find(str);
    if (iter == idMap.end()){
      //std::cerr << "unknown MPI call " << str << "in ID map" << std::endl;
      //spkt_abort_printf("unknown MPI call %s in ID map", str.c_str());
      return (MPI_CALL_ID)(0);
    }
    return iter->second;
  }
  MPINameIDMap();
};

extern MPINameIDMap MPI_call_to_id;

#endif /* STRUCTURES_H_ */