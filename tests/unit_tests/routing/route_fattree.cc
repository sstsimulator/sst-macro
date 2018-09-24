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

#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/fat_tree_router.h>
#include <sprockit/util.h>

extern void test_topology(sprockit::sim_parameters& params);

void
test_fattree4(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["num_core_switches"] = "4";
  params["down_ports_per_core_switch"] = "16";
  params["num_agg_subtrees"] = "4";
  params["agg_switches_per_subtree"] = "4";
  params["up_ports_per_agg_switch"] = "4";
  params["down_ports_per_agg_switch"] = "4";
  params["leaf_switches_per_subtree"] = "4";
  params["up_ports_per_leaf_switch"] = "4";
  params["router.name"] = "fat_tree";
  params["name"] = "fat_tree";
  test_topology(params);
}

void
test_fattree_min(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["num_core_switches"] = "4";
  params["down_ports_per_core_switch"] = "16";
  params["num_agg_subtrees"] = "4";
  params["agg_switches_per_subtree"] = "4";
  params["up_ports_per_agg_switch"] = "4";
  params["down_ports_per_agg_switch"] = "4";
  params["leaf_switches_per_subtree"] = "4";
  params["up_ports_per_leaf_switch"] = "4";
  params["router.name"] = "fat_tree_minimal";
  params["name"] = "fat_tree";
  test_topology(params);
}
