![SST](http://sst-simulator.org/img/sst-logo-small.png)

# Structural Simulation Toolkit (SST) Macroscale Element Library

#### Copyright (c) 2009-2017, Sandia National Laboratories
Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

---

The Structural Simulation Toolkit (SST) was developed to explore innovations in highly concurrent systems where the ISA, microarchitecture, and memory interact with the programming model and communications system. The package provides two novel capabilities. The first is a fully modular design that enables extensive exploration of an individual system parameter without the need for intrusive changes to the simulator. The second is a parallel simulation environment based on MPI. This provides a high level of performance and the ability to look at large systems. The framework has been successfully used to model concepts ranging from processing in memory to conventional processors connected by conventional network interfaces and running MPI.

The macroscale element library provides functionality for simulating extreme-scale applications.  Extreme scale is achieved through: 

* Skeletonized endpoint models that provide accurate, but fast models of applications.
* Coarse-grained networks models that capture important traffic contention without modeling flit-level details
* Parallelization through a PDES (parallel discrete event simulation) core

The most unique functionality is dedicated to skeleton applications which run as emulated processes on a barebones virtual OS.  Each process (and thread) is modeled as a user-space thread, allowing millions of virtual simulation processes to be modeled in a single simulation.

---

To learn more, see the PDF manuals in the top-level source directory. 
Visit [sst-simulator.org](http://sst-simulator.org) to learn more about SST core.

##### [LICENSE](https://github.com/sstsimulator/sst-core/blob/devel/LICENSE)

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Under the terms of Contract DE-NA0003525 with Sandia Corporation, 
the U.S. Government retains certain rights in this software.

