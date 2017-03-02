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

#ifndef STRUCTURES_H_
#define STRUCTURES_H_

#include <string>
#include <unordered_map>
#include <otf2/otf2.h>

using namespace std;

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
    OTF2_RegionRole regionRole;
    OTF2_Paradigm paradigm;
};

struct OTF2_Callpath {
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
};

extern unordered_map<string, int> MPI_call_to_id;

#endif /* STRUCTURES_H_ */
