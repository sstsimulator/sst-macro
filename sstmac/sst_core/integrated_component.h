/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#ifndef SSTMAC_MICRO_INTEGRATED_COMPONENT_H_
#define SSTMAC_MICRO_INTEGRATED_COMPONENT_H_

#include <sstmac/common/sstmac_config.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/link.h>
#include <sst/core/linkMap.h>
#include <sst/core/params.h>
#include <sst/core/component.h>
#include <sst/core/subcomponent.h>
#include <sst/core/eli/elibase.h>

#define SSTMAC_VALID_PORTS \
   {"input%(in)d",  "Will receive new payloads here",      {}}, \
   {"output%(out)d", "Will receive new acks(credits) here", {}}, \
   {"rtr",                   "Special link to Merlin router",       {}}


#define SST_ELI_REGISTER_DERIVED_COMPONENT(base,cls,lib,name,version,desc,cat) \
  SST_ELI_REGISTER_COMPONENT(cls,lib,name,ELI_FORWARD_AS_ONE(version),desc,cat)

#else

#define SST_ELI_DECLARE_BASE(x) \
  SPKT_DECLARE_BASE(x)

#define SST_ELI_DECLARE_CTOR(...) \
  SPKT_DECLARE_CTOR(SPKT_FORWARD_AS_ONE(__VA_ARGS__))

#define SST_ELI_DECLARE_DEFAULT_CTOR() \
  SPKT_DECLARE_DEFAULT_CTOR()

#define SST_ELI_DECLARE_DEFAULT_INFO()

#define SST_ELI_DOCUMENT_STATISTICS(...)

#define SST_ELI_DOCUMENT_SUBCOMPONENT_SLOTS(...)

#define SST_ELI_ELEMENT_VERSION(...)

#define SST_ELI_DOCUMENT_PORTS(...)

//if this macro is used, then this is ONLY registered for core
#define SST_ELI_REGISTER_COMPONENT(cls,lib,name,version,desc,cat)

#define SST_ELI_REGISTER_DERIVED_COMPONENT(base,cls,lib,name,version,desc,cat) \
  SPKT_REGISTER_DERIVED(base,cls,lib,name,desc)

#define SST_ELI_REGISTER_DERIVED(base,cls,lib,name,version,desc) \
  SPKT_REGISTER_DERIVED(base,cls,lib,name,desc)

#define COMPONENT_CATEGORY_UNCATEGORIZED  0x00
#define COMPONENT_CATEGORY_PROCESSOR      0x01
#define COMPONENT_CATEGORY_MEMORY         0x02
#define COMPONENT_CATEGORY_NETWORK        0x04
#define COMPONENT_CATEGORY_SYSTEM         0x08
#endif

#endif /* SSTMAC_MICRO_INTEGRATED_COMPONENT_H_ */
