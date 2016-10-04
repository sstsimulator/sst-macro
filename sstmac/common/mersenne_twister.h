/*
 * Mersenne twister adapted for SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *
 * The original code has the following license information:
 *
 **********************************
 *
 * A C-program for MT19937-64 (2004/9/29 version).
 * Coded by Takuji Nishimura and Makoto Matsumoto.
 *
 * This is a 64-bit version of Mersenne Twister pseudorandom number
 * generator.
 *
 * Before using, initialize the state by using init_genrand64(seed)
 * or init_by_array64(init_key, key_length).
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * References:
 * T. Nishimura, ``Tables of 64-bit Mersenne Twisters''
 *   ACM Transactions on Modeling and
 *   Computer Simulation 10. (2000) 348--357.
 * M. Matsumoto and T. Nishimura,
 *   ``Mersenne Twister: a 623-dimensionally equidistributed
 *     uniform pseudorandom number generator''
 *   ACM Transactions on Modeling and
 *   Computer Simulation 8. (Jan. 1998) 3--30.
 *
 * Any feedback is very welcome.
 * http://www.math.hiroshima-u.ac.jp/~m-mat/MT/emt.html
 * email: m-mat @ math.sci.hiroshima-u.ac.jp (remove spaces)
 *
 ***************************************
 *
 * Modest changes made to adapt the code for SST/macroscale:
 * 1) Global (static) state eliminated -- state vector is now member data.
 * 2) Code moved into namespace sstmac::RNG and put in a class derived from
 *    UniformInteger.
 * 3) Some function signatures and types renamed to better match function
 *    names in SST/macro.
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

