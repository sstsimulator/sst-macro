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

#ifndef SSTMAC_COMMON_MERSENNETWISTER_H_INCLUDED
#define SSTMAC_COMMON_MERSENNETWISTER_H_INCLUDED

#include <sstmac/common/rng.h>
#include <stdint.h>



namespace RNG {

/**
 * Updated Mersenne twister.
 *
 * Copyright (C) 2004, Makoto Matsumoto and Takuji Nishimura,
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. The names of its contributors may not be used to endorse or promote
 *      products derived from this software without specific prior written
 *      permission.
 *
 * Modified to fit into the SST/macroscale code structure
 * and to eliminate global (static) state.
 */
class mersenne_twister : public UniformInteger
{
  /// State vector length defaults to 312 entries ('NN' in original code).
  static const int state_vector_length_;
  /// I believe this is a mask length, defaults to 156 ('MM' in original).
  static const int mask_length_;
  /// The array for the state vector.  Of length 'state_vector_length_'.
  std::vector<uint64_t> mt_;
  /// The current position in the state vector ('mti' in original code).
  size_t mti_;

  void reseed(uint64_t seed);

  /// Like normally for SST/macro types, constructor is private.
  mersenne_twister(uint64_t seed);

 public:
  /// Create a new twister.
  static mersenne_twister* construct(uint64_t seed);

  /// Cleanup.
  virtual ~mersenne_twister();

  /// Get a value in the interval [0,numeric_limits<rngint_t>::max()]
  virtual rngint_t value() override;

  /// Reseed the generator.  This is an expensive operation.
  virtual void vec_reseed(const std::vector<rngint_t> &seeds) override;

  /// The number of seeds employed by this generator.
  /// The generator will accept any value up to state_vector_length_.
  virtual int nseed() override {
    return state_vector_length_;
  }
};

} // end of namespace RNG.



#endif