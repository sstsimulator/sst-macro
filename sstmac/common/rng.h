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

#ifndef SSTMAC_COMMON_RNG_H_INCLUDED
#define SSTMAC_COMMON_RNG_H_INCLUDED

#include <stdint.h>
#include <limits>
#include <vector>
#include <string>
#include <ctime>

namespace RNG {

typedef uint32_t rngint_t;

/** This is a base class for random number generators that return
 an integer uniformly distributed in a range. */
class UniformInteger
{
 public:
  virtual
  ~UniformInteger();

  virtual rngint_t
  value() = 0;

  rngint_t
  value_in_range(rngint_t range) {
    return value() % range;
  }

  virtual void
  vec_reseed(const std::vector<rngint_t> &seeds) = 0;

  virtual int
  nseed()= 0;

  /// Return a random value in the interval [0,1], (0,1], [0,1), or (0,1)
  virtual double
  realvalue(bool include_zero = true, bool include_one = true) {
    rngint_t v = this->value();
    // Total precision of 64-bit double is 53 bits --
    // avoid oddball rounding errors.
    double scaling = std::numeric_limits<rngint_t>::max();
    if (sizeof(rngint_t) > 6) {
      int shiftbits = 8 * int(sizeof(rngint_t)) - 53;
      v >>= shiftbits;
      scaling = 9007199254740992.0; // 2**53
    }
    if (!include_zero) {
      v = (v >> 1) + 1;
      scaling = scaling / 2.0 + 1.0;
    }
    if (include_one) {
      scaling -= 1.0;
    }
    return v / scaling;
  }

  void
  reseed();

  void
  reseed(rngint_t);

  void
  reseed(rngint_t, rngint_t);

  void
  reseed(rngint_t, rngint_t, rngint_t);

  void
  reseed(rngint_t, rngint_t, rngint_t, rngint_t);
};

/** The multiple-with-carry random number generator by
 George Marsaglia (1999; internet posting).

 This RNG is fast and has a low memory footprint.
 Here is an excerpt from the posting describing this generator:
 <i>The MWC generator concatenates two 16-bit multiply-with-carry
 generators, x(n)=36969x(n-1)+carry, y(n)=18000y(n-1)+carry mod
 2<sup>16</sup>, has period about 2<sup>60</sup> and seems to pass all tests of
 randomness. A favorite stand-alone generator---faster than
 KISS, which contains it.</i>
 */
class MWC : public UniformInteger
{
 private:
  static const rngint_t default_z, default_w;
  rngint_t z, w;

  rngint_t
  znew() {
    return ((z = 36969 * (z & 65535) + (z >> 16)) << 16);
  }

  rngint_t
  wnew() {
    return ((w = 18000 * (w & 65535) + (w >> 16)) & 65535);
  }

 protected:
  MWC();

 public:
  static MWC*
  construct();

  static MWC*
  construct(const std::vector<rngint_t> &);

  static MWC*
  construct(rngint_t zarg);

  static MWC*
  construct(rngint_t zarg, rngint_t warg);

  ~MWC();

  rngint_t
  value() override {
    return (znew() + wnew());
  }

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int nseed() override;
};

/** The 3-shift-register random number generator by George
 Marsaglia (1999; internet posting).

 This RNG is fast and has a very low memory footprint.
 Here is an excerpt from the posting describing this generator:
 <i>SHR3 is a 3-shift-register generator with period 2<sup>32</sup>-1. It
 uses y(n)=y(n-1)(I+L17)(I+R13)(I+L5), with the y's viewed as
 binary vectors, L the 32x32 binary matrix that shifts a vector
 left 1, and R its transpose.  SHR3 seems to pass all except
 the binary rank test, since 32 successive values, as binary
 vectors, must be linearly independent, while 32 successive
 truly random 32-bit integers, viewed as binary vectors, will
 be linearly independent only about 29% of the time.</i>
 */
class SHR3 : public UniformInteger
{
 private:
  rngint_t jsr;

 protected:
  SHR3();

 public:
  static SHR3*
  construct();

  static SHR3*
  construct(const std::vector<rngint_t>&);

  static SHR3*
  construct(rngint_t jsrarg);

  ~SHR3();

  rngint_t value() override;

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int nseed() override;
};

class ExponentialDistribution
{
 private:
  SHR3* rand_;

  double lambda_;

 public:
  ExponentialDistribution(double lambda) :
    lambda_(lambda)
  {
    rngint_t seed = time(NULL);
    rand_ = SHR3::construct(seed);
  }

  double value();

};

class NormalDistribution
{
 private:
  SHR3* rand_;

  double mean_;

  double stdev_;

  double maxZ_;

  bool reuse_;

  double Y_;

 public:
  NormalDistribution(double mean, double stdev,
                     double maxZ = 2.0, rngint_t seed = 0);

  ~NormalDistribution();

  double value();
};

/** The congruential random number generator by George Marsaglia
 (1999; internet posting).

 This RNG is fast and has a very low memory footprint.
 Here is an excerpt from the posting describing this generator:
 <i>CONG is a congruential generator with the widely used 69069
 as multiplier: x(n)=69069x(n-1)+1234567.  It has period 2<sup>32</sup>.
 The leading half of its 32 bits seem to pass all tests, but
 bits in the last half are too regular.</i>
 */
class CONG : public UniformInteger
{
 private:
  rngint_t jcong;

 public:
  static CONG*
  construct();

  static CONG*
  construct(const std::vector<rngint_t>&);

  static CONG*
  construct(rngint_t jcongarg);

  ~CONG();

  rngint_t
  value() override {
    return (jcong = 69069 * jcong + 1234567);
  }

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int
  nseed() override;

 protected:
  CONG();

  CONG(const std::vector<rngint_t>&);

  CONG(rngint_t jcongarg);
};

/** A simple random generator using MWC, CONG, and SHR3 by
 George Marsaglia (1999; internet posting).

 Here is an excerpt from the posting describing this generator:
 <i>The KISS generator, (Keep It Simple Stupid), is designed to
 combine the two multiply-with-carry generators in MWC with the
 3-shift register SHR3 and the congruential generator CONG,
 using addition and exclusive-or. Period about 2<sup>123</sup>. It is one
 of my favorite generators.</i>
 */
class SimpleCombo : public UniformInteger
{
 private:
  MWC* mwc_;
  CONG* cong_;
  SHR3* shr3_;

 public:
  static SimpleCombo*
  construct();

  static SimpleCombo*
  construct(const std::vector<rngint_t> &);

  static SimpleCombo*
  construct(rngint_t zarg);

  static SimpleCombo*
  construct(rngint_t zarg, rngint_t warg);

  static SimpleCombo*
  construct(rngint_t zarg, rngint_t warg, rngint_t jsrarg);

  static SimpleCombo*
  construct(rngint_t zarg, rngint_t warg, rngint_t jsrarg, rngint_t jcongarg);

  ~SimpleCombo();

  // value is written in this way to make it possible to inline
  // the MWC, CONG, and SHR3 value calls.
  rngint_t
  value() override {
    return (mwc_->MWC::value() ^ cong_->CONG::value()) + shr3_->SHR3::value();
  }

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int
  nseed() override;

 protected:
  SimpleCombo();
};

/** A base class for random number generators using a table of 256 32-bit integers. */
class Table256 : public UniformInteger
{
 public:
  ~Table256();

  void
  vec_reseed(const std::vector<rngint_t> &seeds);

  int
  nseed();

 protected:
  UniformInteger* seeder_;

  rngint_t t[256];

  unsigned char c;

  Table256();

};

/**
 A lagged Fibonacci random number generator by George Marsaglia
 (1999; internet posting).

 Here is an excerpt from the posting describing this generator:
 <i>LFIB4 is an extension of the class that I have previously
 defined as lagged Fibonacci generators: x(n)=x(n-r) op x(n-s),
 with the x's in a finite set over which there is a binary
 operation op, such as +,- on integers mod 232, * on odd such
 integers, exclusive-or (xor) on binary vectors. Except for
 those using multiplication, lagged Fibonacci generators fail
 various tests of randomness, unless the lags are very long.  To
 see if more than two lags would serve to overcome the problems
 of 2- lag generators using +,- or xor, I have developed the
 4-lag generator LFIB4: x(n)=x(n-256)+x(n-179)+x(n-119)+x(n-55)
 mod 232.  Its period is 2<sup>31</sup>*(2<sup>256</sup>-1), about 2<sup>287</sup>, and it seems
 to pass all tests---in particular, those of the kind for which
 2-lag generators using +,-,xor seem to fail.  For even more
 confidence in its suitability, LFIB4 can be combined with KISS,
 with a resulting period of about 2<sup>410</sup>: just use (KISS+LFIB4) in
 any C expression.</i>
 */
class LFIB4 : public Table256
{
 protected:
  LFIB4();

 public:
  static LFIB4*
  construct();

  static LFIB4*
  construct(const std::vector<rngint_t> &seeds);

  rngint_t
  value() override {
    unsigned char i1, i2, i3, i4;
    i1 = c;
    i2 = c + 58;
    i3 = c + 119;
    i4 = ++c + 178;
    return (t[i1] = t[i1] + t[i2] + t[i3] + t[i4]);
  }

  ~LFIB4() ;

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int
  nseed() override;
};

/**
 A subtract with borrow random number generator by George
 Marsaglia (1999; internet posting).

 Here is an excerpt from the posting describing this generator:
 <i>SWB is a subtract-with-borrow generator that I developed to
 give a simple method for producing extremely long periods:
 x(n)=x(n-222)-x(n-237)-borrow mod 2<sup>32</sup>.  The 'borrow' is 0
 unless set to 1 if computing x(n-1) caused overflow in 32-bit
 integer arithmetic. This generator has a very long period,
 2<sup>7098</sup>(2<sup>480</sup>-1), about 2<sup>7578</sup>. It seems to pass all tests of
 randomness, but, suspicious of a generator so simple and fast
 (62 nanosecs at 300MHz), I would suggest combining SWB with
 KISS, MWC, SHR3, or CONG.</i>
 */
class SWB : public Table256
{
 private:
  rngint_t x, y;

 protected:
  SWB();

 public:
  static SWB*
  construct();

  static SWB*
  construct(const std::vector<rngint_t>&);

  rngint_t
  value() override;

  ~SWB();

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int
  nseed() override;
};

/**
 A random number generator combining several techniques by
 George Marsaglia (1999; internet posting).

 Here is an excerpt from the posting describing this generator:
 <i>For the super cautious, (KISS+SWB) in an expression would
 provide a random 32-bit integer from a sequence with period >
 2<sup>7700</sup>, and would only add some 300 nanoseconds to the computing
 time for that expression.</i>
 */

class Combo : public UniformInteger
{
 public:
  static Combo*
  construct();

  static Combo*
  construct(const std::vector<rngint_t>&);

  rngint_t
  value() override {
    rngint_t kresult = simplecombo_->SimpleCombo::value();
    rngint_t sresult = swb_->SWB::value();
    return kresult + sresult;
  }

  ~Combo();

  void
  vec_reseed(const std::vector<rngint_t> &seeds) override;

  int
  nseed() override;

 private:
  SWB* swb_;
  SimpleCombo* simplecombo_;

 protected:
  Combo();

};

/** Converts a shared* to a RNG to a functor. */
class UniformInteger_functor
{
  UniformInteger* p_;

 public:
  UniformInteger_functor(UniformInteger* p) :
    p_(p) {
  }

  rngint_t
  operator()() {
    return p_->value();
  }

  rngint_t
  operator()(rngint_t range) {
    return p_->value_in_range(range);
  }
};

}

#endif