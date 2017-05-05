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

struct OTF2_LocationGroup {
  OTF2_StringRef name;
  OTF2_LocationGroupType locationGroupType;
  OTF2_SystemTreeNodeRef systemTreeParent;
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
  std::vector<uint64_t> members;
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
