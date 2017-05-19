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

#include <sstmac/common/mersenne_twister.h>

namespace RNG {

#define MATRIX_A 0xB5026F5AA96619E9ULL
#define UM 0xFFFFFFFF80000000ULL /* Most significant 33 bits */
#define LM 0x7FFFFFFFULL /* Least significant 31 bits */

//
// Static data.
//
const int mersenne_twister::state_vector_length_ = 312;
const int mersenne_twister::mask_length_ = 156;

//
// Re-populate the state vector.
//
void mersenne_twister::reseed(uint64_t seed)
{
  mt_.resize(state_vector_length_);
  mt_.at(0) = seed;
  for(mti_ = 1; mti_ < mt_.size(); ++mti_)
    mt_.at(mti_) =  (6364136223846793005ULL *
                     (mt_.at(mti_ - 1) ^ (mt_.at(mti_ - 1) >> 62)) +
                     mti_);
}

//
// Like normally for SST/macro types, constructor is private.
//
mersenne_twister::mersenne_twister(uint64_t seed)
{
  reseed(seed);
}

//
// Create a new twister.
//
mersenne_twister*
mersenne_twister::construct(uint64_t seed)
{
  return new mersenne_twister(seed);
}

//
// Cleanup.
//
mersenne_twister::~mersenne_twister()
{
}

//
// Get a value in the interval [0,numeric_limits<rngint_t>::max()]
//
rngint_t mersenne_twister::value()
{
  int i;
  uint64_t x;
  static uint64_t mag01[2]= {0ULL, MATRIX_A};

  if (mti_ >= mt_.size()) { // generate state_vector_length_ words at a time
    for (i=0; i<state_vector_length_ - mask_length_; i++) {
      x = (mt_.at(i)&UM)|(mt_.at(i+1)&LM);
      mt_.at(i) = mt_.at(i+mask_length_) ^ (x>>1) ^ mag01[(int)(x&1ULL)];
    }
    for (; i<state_vector_length_-1; i++) {
      x = (mt_.at(i)&UM)|(mt_.at(i+1)&LM);
      mt_.at(i) = (mt_.at(i+(mask_length_ - state_vector_length_)) ^
                   (x>>1) ^ mag01[(int)(x&1ULL)]);
    }
    x = (mt_.at(state_vector_length_-1)&UM)|(mt_.at(0)&LM);
    mt_.at(state_vector_length_-1) = (mt_.at(mask_length_-1) ^
                                      (x>>1) ^ mag01[(int)(x&1ULL)]);
    mti_ = 0;
  }

  x = mt_.at(mti_++);

  x ^= (x >> 29) & 0x5555555555555555ULL;
  x ^= (x << 17) & 0x71D67FFFEDA60000ULL;
  x ^= (x << 37) & 0xFFF7EEE000000000ULL;
  x ^= (x >> 43);

  return rngint_t(x);
}

//
// Reseed the generator.  This is an expensive operation.
//
void mersenne_twister::vec_reseed(const std::vector<rngint_t> &seeds)
{
  reseed(19650218ULL);
  size_t k = ( mt_.size() > seeds.size() ? mt_.size() : seeds.size());
  size_t i = 1, j = 0;
  for (; k > 0; k--) {
    mt_.at(i) = ((mt_.at(i) ^ ((mt_.at(i-1) ^ (mt_.at(i-1) >> 62)) *
                               3935559000370003845ULL))
                 + seeds.at(j) + j); /* non linear */
    i++;
    j++;
    if (i>=mt_.size()) {
      mt_.at(0) = mt_.at(mt_.size()-1);
      i=1;
    }
    if (j>=seeds.size()) {
      j=0;
    }
  }
  for (k=mt_.size()-1; k > 0; k--) {
    mt_.at(i) = ((mt_.at(i) ^ ((mt_.at(i-1) ^ (mt_.at(i-1) >> 62)) *
                               2862933555777941757ULL))
                 - i); /* non linear */
    i++;
    if (i>=mt_.size()) {
      mt_.at(0) = mt_.at(mt_.size()-1);
      i=1;
    }
  }

  mt_.at(0) = 1ULL << 63; /* MSB is 1; assuring non-zero initial array */
}


} // end of namespace RNG