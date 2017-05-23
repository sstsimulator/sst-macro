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

#ifndef sstmac_software_process_PMI_H
#define sstmac_software_process_PMI_H

#include <sstmac/common/node_address.h>
#include <sstmac/software/process/software_id.h>

#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

#include <sprockit/unordered.h>

namespace sstmac {
namespace sw {

class process_manager  {

 public:
  process_manager(software_id sid, operating_system* os);

  virtual ~process_manager();

  int
  get_partner(node_id addr) const;

  node_id
  my_addr() const {
    return my_addr_;
  }

  void kill_node();

  void kill_process();

 private:
  typedef spkt_unordered_map<int, node_id> proc_to_node_map;

  typedef spkt_unordered_map<int, proc_to_node_map> app_to_proc_to_node_map;

  typedef spkt_unordered_map<node_id, int> node_to_proc_map;

  typedef spkt_unordered_map<int, node_to_proc_map> app_to_node_to_proc_map;

  static app_to_proc_to_node_map node_map_;

  static app_to_node_to_proc_map proc_map_;

  node_id my_addr_;

  operating_system* my_os_;

  software_id sid_;


};

}
}



#endif // PMI_H