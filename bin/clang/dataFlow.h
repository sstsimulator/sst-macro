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
#ifndef bin_clang_dataFlow_h
#define bin_clang_dataFlow_h

struct DataFlowMap {
  struct Access {
    uint32_t generation;
  };

  std::map<clang::NamedDecl*,uint32_t> generation;

  std::set<uint64_t> accessed;
  static void
  append(uint64_t& id, uint16_t modifier, uint16_t idx){
    id |= (modifier << idx);
  }
  uint16_t id_count;
  DataFlowMap() : id_count(0) {}
};


struct MemoryLocation {
  char label[128];

  template <class T>
  void
  append(const T& t){
    //::memcpy(&label[size], &t, sizeof(T));
    T* arr = (T*) &label[size];
    *arr = t;
    size += sizeof(T);
  }

  const char* c_str() const {
    char* ret = new char[128];
    int writeIdx = 0;
    for (int i=0; i < size; ++i){
      if (label[i] != '\0'){
        ret[writeIdx++] = label[i];
      }
    }
    ret[writeIdx] = '\0';
    return ret;
  }

  void
  updateGeneration(uint32_t gen){
    maxGen = std::max(maxGen, gen);
  }

  void
  append(const MemoryLocation& mloc){
    ::memcpy(&label[size], mloc.label, mloc.size);
    size += mloc.size;
    maxGen = std::max(maxGen, mloc.maxGen);
  }

  uint32_t maxGen;
  uint8_t size;
  MemoryLocation() : maxGen(0), size(0) {}
};

struct MemoryLocationCompare {
  bool operator()(const MemoryLocation& lhs, const MemoryLocation& rhs) const {
    if (lhs.size != rhs.size) return lhs.size < rhs.size;
    int cmp = memcmp(lhs.label, rhs.label, lhs.size);
    return cmp < 0;
  }
};

struct AccessHistory {
  uint32_t lastReadGeneration;
  uint32_t lastWriteGeneration;

  AccessHistory() : lastReadGeneration(0), lastWriteGeneration(0)
  {
  }

  bool newAccess(uint32_t maxGenDependence, uint32_t currentGeneration, bool isLHS){
    if (isLHS){
      bool updated = lastWriteGeneration < maxGenDependence || lastWriteGeneration == 0;
      lastWriteGeneration = currentGeneration;
      return updated;
    } else {
      bool updated = lastReadGeneration < maxGenDependence || lastReadGeneration == 0;
      lastReadGeneration = currentGeneration;
      return updated;
    }
  }
};

#endif
