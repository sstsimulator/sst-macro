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

#include <sprockit/errors.h>
#include <sstmac/libraries/pthread/sstmac_sched.h>

/* Set scheduling parameters for a process.  */
extern "C" int
SSTMAC_sched_setparam (pid_t pid, const struct sched_param *param){
  spkt_throw(sprockit::unimplemented_error, "sched_setparam");
  return 0;
}

/* Retrieve scheduling parameters for a particular process.  */
extern "C" int
SSTMAC_sched_getparam (pid_t pid, struct sched_param *param){
  spkt_throw(sprockit::unimplemented_error, "sched_getparam");
  return 0;
}

/* Set scheduling algorithm and/or parameters for a process.  */
extern "C" int
SSTMAC_sched_setscheduler (pid_t pid, int policy,  const struct sched_param *param){
  spkt_throw(sprockit::unimplemented_error, "sched_setscheduler");
  return 0;
}

/* Retrieve scheduling algorithm for a particular purpose.  */
extern "C" int
SSTMAC_sched_getscheduler (pid_t pid){
  spkt_throw(sprockit::unimplemented_error, "sched_getscheduler");
  return 0;
}

/* Yield the processor.  */
extern "C" int
SSTMAC_sched_yield (void){
  //for now make it a no op
  return 0;
}


/* Get the SCHED_RR interval for the named process.  */
extern "C" int
SSTMAC_sched_rr_get_interval (pid_t pid, struct timespec *t){
  spkt_throw(sprockit::unimplemented_error, "sched_rr_get_interval");
  return 0;
}

extern "C" void
SSTMAC_CPU_SET_S (int cpu, size_t setsize, sstmac_cpu_set_t* cpusetp){
  if (setsize > SSTMAC_CPU_SETSIZE){
    spkt_throw(sprockit::value_error,
      "SSTMAC_CPU_SET: invalid cpu setsize %lu", setsize);
  }
  cpusetp->cpubits = cpusetp->cpubits | (1<<cpu);
}

extern "C" void
SSTMAC_CPU_CLR_S (int cpu, size_t setsize, sstmac_cpu_set_t* cpusetp){
  if (setsize > SSTMAC_CPU_SETSIZE){
    spkt_throw(sprockit::value_error,
      "SSTMAC_CPU_CLR: invalid cpu setsize %lu", setsize);
  }
  cpusetp->cpubits = cpusetp->cpubits & ~(1<<cpu);
}

extern "C" int
SSTMAC_CPU_ISSET_S (int cpu, size_t setsize, sstmac_cpu_set_t* cpusetp){
  return cpusetp->cpubits & (1<<cpu);
}

extern "C" void
SSTMAC_CPU_ZERO_S (size_t setsize, sstmac_cpu_set_t* cpusetp){
  if (setsize > SSTMAC_CPU_SETSIZE){
    spkt_throw(sprockit::value_error,
      "SSTMAC_CPU_CLR: invalid cpu setsize %lu", setsize);
  }
  cpusetp->cpubits = 0;
}

static int
SSTMAC_count_bits(sstmac_cpu_set_t* cpusetp){
  int count = 0;
  for (int i=0; i < SSTMAC_CPU_SETSIZE; ++i){
    if (cpusetp->cpubits & (1<<i)){
      ++count;
    }
  }
  return count;
}

extern "C" int
SSTMAC_CPU_COUNT_S (size_t setsize, sstmac_cpu_set_t* cpusetp){
  return SSTMAC_count_bits(cpusetp);
}

extern "C" int
SSTMAC_CPU_EQUAL_S(size_t setsize, sstmac_cpu_set_t* cpusetp1, sstmac_cpu_set_t* cpusetp2){
  return cpusetp1->cpubits == cpusetp1->cpubits;
}

extern "C" void
SSTMAC_CPU_AND_S(size_t setsize, sstmac_cpu_set_t* destset, sstmac_cpu_set_t* srcset1, sstmac_cpu_set_t* srcset2){
  destset->cpubits = srcset1->cpubits & srcset2->cpubits;
}

extern "C" void
SSTMAC_CPU_OR_S(size_t setsize, sstmac_cpu_set_t* destset, sstmac_cpu_set_t* srcset1, sstmac_cpu_set_t* srcset2){
  destset->cpubits = srcset1->cpubits | srcset2->cpubits;
}

extern "C" void
SSTMAC_CPU_XOR_S(size_t setsize, sstmac_cpu_set_t* destset, sstmac_cpu_set_t* srcset1, sstmac_cpu_set_t* srcset2){
  destset->cpubits = srcset1->cpubits ^ srcset2->cpubits;
}

extern "C" sstmac_cpu_set_t*
SSTMAC_CPU_ALLOC(int count){
  if (count > SSTMAC_CPU_SETSIZE){
    return NULL;
  }
  return new sstmac_cpu_set_t;
}

void SSTMAC_CPU_FREE(sstmac_cpu_set_t* cpuset){
  delete cpuset;
}


/* Set the CPU affinity for a task */
extern "C" int
SSTMAC_sched_setaffinity (pid_t pid, size_t cpusetsize, const sstmac_cpu_set_t *cpuset){
  spkt_throw(sprockit::unimplemented_error,
    "SSTMAC_sched_setaffinity");
}

/* Get the CPU affinity for a task */
extern "C" int
SSTMAC_sched_getaffinity (pid_t pid, size_t cpusetsize, sstmac_cpu_set_t *cpuset){
  spkt_throw(sprockit::unimplemented_error,
    "SSTMAC_sched_getaffinity");
}