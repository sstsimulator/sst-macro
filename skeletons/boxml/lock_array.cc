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

#include "lock_array.h"

uint32_t
hash(int index)
{
  //jenkins hash the integer
  char* key = reinterpret_cast<char*>(&index);
  const int length = sizeof(uint32_t);
  uint32_t hash = 0;
  for(int i = 0; i < length; ++i)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

void
lock_array::init(int nlocks)
{
  num_locks_ = nlocks;
  locks_.resize(nlocks);
  for (int i=0; i < nlocks; ++i){
    locks_[i] = sstmac::sim_thread_lock::construct();
	}
}

void
lock_array::lock(int index)
{
  uint32_t idx = hash(index) % num_locks_;
  locks_[idx]->lock();
}

void
lock_array::unlock(int index)
{
  uint32_t idx = hash(index) % num_locks_;
  locks_[idx]->unlock();
}

void
lock_array::print_scatter(int nIndex)
{
  std::vector<int> counts(num_locks_);
  for (int i=0; i < nIndex; ++i){
    int idx = hash(i) % num_locks_;
    printf("A[%d]=%d\n", i, idx);
    counts[idx]++;
  }

  for (int i=0; i < num_locks_; ++i){
    printf("Total[%d]=%d\n", i, counts[i]);
  }

}