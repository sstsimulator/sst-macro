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

#ifndef CONNECTABLE_WRAPPER_H
#define CONNECTABLE_WRAPPER_H

#include <sstmac/common/sstmac_config.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/hardware/common/connection.h>
#include <sprockit/unordered.h>
#include <sst/core/link.h>
#include <Python.h>

namespace sstmac {

/**
 * @brief The link_wrapper class  SST/macro is built almost entirely around event handlers.
 * In the integrated core, Links serve the purpose of event handlers.
 * This wraps an SST link inside an event handler to preserve the sst/macro API.
 */
class link_wrapper :
    public event_handler
{
  public:
    std::string
    to_string() const override {
      return "link_wrapper";
    }

    link_wrapper(SST::Link* link) :
        event_handler(device_id()),
        link_(link)
    {
    }

    void
    handle(event* ev) override {
      spkt_throw(sprockit::unimplemented_error,
        "link_wrapper::handle: should never be called");
    }

    SST::Link*
    link() const override {
      return link_;
    }

  private:
    SST::Link* link_;
};

}
#endif

#endif // CONNECTABLE_WRAPPER_H