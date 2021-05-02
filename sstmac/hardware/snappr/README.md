![SST](http://sst-simulator.org/img/sst-logo-small.png)

# Structural Simulation Toolkit (SST) Macroscale Element Library

#### Copyright (c) 2009-2021, Sandia National Laboratories
Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

---

# SNAPPR Model Configuration

## Quality-of-Service (QoS) simulations

There are three types of QoS possible
* Priority
* Priority + Bandwidth Minimum
* Priority + Bandwidth Maximum
When arbitrating which packet to send next on a given port,
packets are selected based on priority.
For low-priority packets that might be starved out, a bandwidth minimum can be given.
This ensures forward progress on a given port for a packet.
For high-priority packets that might starve out other packts,
a bandwidth maximum can be given.

A QoS policy is selected for each virtual lane.
In the input file, switch link paramters or NIC injection parameters must
specify the virtual lane configuration.

````
switch {
 link {
   vl_types = [none,min,max]
   vl_weights = [0,0.2, 0.75]
   vl_priorities = [1,0,2]
````
Three different arrays are given. The types array specifies the general policy.
The priorities array specifies which virtual lanes to prefer (higher numbers mean higher priority).
The weights array gives either a bandwidth minimum or maximum, depending on the policy.

##### [LICENSE](https://github.com/sstsimulator/sst-core/blob/devel/LICENSE)

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Under the terms of Contract DE-NA0003525 with NTESS, 
the U.S. Government retains certain rights in this software.

