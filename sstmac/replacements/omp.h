/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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
#ifndef sstmac_replacement_omp_h
#define sstmac_replacement_omp_h


#define omp_init_lock sstmac_omp_init_lock
#define omp_destroy_lock sstmac_omp_destroy_lock
#define omp_set_lock sstmac_omp_set_lock
#define omp_unset_lock sstmac_omp_unset_lock
#define omp_test_lock sstmac_omp_test_lock
#define omp_get_num_threads sstmac_omp_get_num_threads
#define omp_get_thread_num sstmac_omp_get_thread_num
#define omp_get_max_threads sstmac_omp_get_max_threads
#define omp_get_wtime sstmac_omp_get_wtime
#define omp_get_num_procs sstmac_omp_get_num_procs
#define omp_set_num_threads sstmac_omp_set_num_threads
#define omp_in_parallel sstmac_omp_in_parallel
#define omp_get_level sstmac_omp_get_level
#define omp_get_ancestor_thread_num sstmac_omp_get_ancestor_thread_num


#define sstmac_omp_lock_t int

#ifdef __cplusplus
extern "C" {
#endif

void sstmac_omp_init_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_destroy_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_set_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_unset_lock(sstmac_omp_lock_t *lock);

int sstmac_omp_test_lock(sstmac_omp_lock_t *lock);

int sstmac_omp_get_thread_num();

int sstmac_omp_get_num_procs();

int sstmac_omp_get_num_threads();

int sstmac_omp_get_max_threads();

void sstmac_omp_set_num_threads(int nthr);

int sstmac_omp_in_parallel();

int sstmac_omp_get_level();

int sstmac_omp_get_ancestor_thread_num();

double sstmac_omp_get_wtime();

#define omp_lock_t sstmac_omp_lock_t

#ifdef __cplusplus
}
#endif

#endif

