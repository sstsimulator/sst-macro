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

#include <sstmac/common/rng.h>
#include <time.h>
#include <cstdlib>
#include <cmath>
#include <stdint.h>

namespace RNG {

////////////////////////////////////////////////////////////////////
// default seeds

static const rngint_t defaultz = 362436069;
static const rngint_t defaultw = 521288629;
static const rngint_t defaultjsr = 123456789;
static const rngint_t defaultjcong = 380116160;

////////////////////////////////////////////////////////////////////
// UniformInteger

UniformInteger::~UniformInteger()
{
}

void
UniformInteger::reseed()
{
  std::vector<rngint_t> s;
  vec_reseed(s);
}

void
UniformInteger::reseed(rngint_t s0)
{
  std::vector<rngint_t> s(1);
  s[0] = s0;
  vec_reseed(s);
}

void
UniformInteger::reseed(rngint_t s0, rngint_t s1)
{
  std::vector<rngint_t> s(2);
  s[0] = s0;
  s[1] = s1;
  vec_reseed(s);
}

void
UniformInteger::reseed(rngint_t s0, rngint_t s1, rngint_t s2)
{
  std::vector<rngint_t> s(3);
  s[0] = s0;
  s[1] = s1;
  s[2] = s2;
  vec_reseed(s);
}

void
UniformInteger::reseed(rngint_t s0, rngint_t s1, rngint_t s2, rngint_t s3)
{
  std::vector<rngint_t> s(4);
  s[0] = s0;
  s[1] = s1;
  s[2] = s2;
  s[3] = s3;
  vec_reseed(s);
}

////////////////////////////////////////////////////////////////////
// MWC

MWC::MWC()
{
}

MWC*
MWC::construct()
{
  MWC* result(new MWC);
  result->reseed();
  return result;
}

MWC*
MWC::construct(const std::vector<rngint_t> &seeds)
{
  MWC* result(new MWC);
  result->vec_reseed(seeds);
  return result;
}

MWC*
MWC::construct(rngint_t zarg)
{
  MWC* result(new MWC);
  result->reseed(zarg);
  return result;
}

MWC*
MWC::construct(rngint_t zarg, rngint_t warg)
{
  MWC* result(new MWC);
  result->reseed(zarg, warg);
  return result;
}

MWC::~MWC()
{
}

void
MWC::vec_reseed(const std::vector<rngint_t> &seeds)
{
  if (seeds.size() > 0) {
    z = seeds[0];
  }
  else {
    z = defaultz;
  }
  if (seeds.size() > 1) {
    w = seeds[1];
  }
  else {
    w = defaultw;
  }
}

int
MWC::nseed()
{
  return 2;
}

////////////////////////////////////////////////////////////////////
// SHR3

SHR3::SHR3()
{
}

SHR3*
SHR3::construct()
{
  SHR3* result(new SHR3);
  result->reseed();
  return result;
}

SHR3*
SHR3::construct(const std::vector<rngint_t> &seeds)
{
  SHR3* result(new SHR3);
  result->vec_reseed(seeds);
  return result;
}

SHR3*
SHR3::construct(rngint_t jsrarg)
{
  SHR3* result(new SHR3);
  result->reseed(jsrarg);
  return result;
}

SHR3::~SHR3()
{
}

void
SHR3::vec_reseed(const std::vector<rngint_t> &seeds)
{
  if (seeds.size() > 0) {
    jsr = seeds[0];
  }
  else {
    jsr = defaultjsr;
  }
}

int
SHR3::nseed()
{
  return 1;
}

////////////////////////////////////////////////////////////////////
// CONG

CONG::CONG()
{
}

CONG*
CONG::construct()
{
  CONG* result(new CONG);
  result->reseed();
  return result;
}

CONG*
CONG::construct(const std::vector<rngint_t> &seeds)
{
  CONG* result(new CONG);
  result->vec_reseed(seeds);
  return result;
}

CONG*
CONG::construct(rngint_t jcongarg)
{
  CONG* result(new CONG);
  result->reseed(jcongarg);
  return result;
}

CONG::~CONG()
{
}

void
CONG::vec_reseed(const std::vector<rngint_t> &seeds)
{
  if (seeds.size() > 0) {
    jcong = seeds[0];
  }
  else {
    jcong = defaultjcong;
  }
}

int
CONG::nseed()
{
  return 1;
}

////////////////////////////////////////////////////////////////////
// SimpleCombo

SimpleCombo::SimpleCombo()
{
  mwc_ = MWC::construct();
  cong_ = CONG::construct();
  shr3_ = SHR3::construct();
}

SimpleCombo*
SimpleCombo::construct()
{
  SimpleCombo* result(new SimpleCombo);
  result->reseed();
  return result;
}

SimpleCombo*
SimpleCombo::construct(const std::vector<rngint_t> &seeds)
{
  SimpleCombo* result(new SimpleCombo);
  result->vec_reseed(seeds);
  return result;
}

SimpleCombo*
SimpleCombo::construct(rngint_t zarg)
{
  SimpleCombo* result(new SimpleCombo);
  result->reseed(zarg);
  return result;
}

SimpleCombo*
SimpleCombo::construct(rngint_t zarg, rngint_t warg)
{
  SimpleCombo* result(new SimpleCombo);
  result->reseed(zarg, warg);
  return result;
}

SimpleCombo*
SimpleCombo::construct(rngint_t zarg, rngint_t warg, rngint_t jsrarg)
{
  SimpleCombo* result(new SimpleCombo);
  result->reseed(zarg, warg, jsrarg);
  return result;
}

SimpleCombo*
SimpleCombo::construct(rngint_t zarg, rngint_t warg, rngint_t jsrarg,
                       rngint_t jcongarg)
{
  SimpleCombo* result(new SimpleCombo);
  result->reseed(zarg, warg, jsrarg, jcongarg);
  return result;
}

SimpleCombo::~SimpleCombo()
{
  delete mwc_;
  delete cong_;
  delete shr3_;
}

void
SimpleCombo::vec_reseed(const std::vector<rngint_t> &seeds)
{
  std::vector<rngint_t> MWCseeds, SHR3seeds, CONGseeds;

  unsigned int i = 0;
  for (int j = 0; j < mwc_->nseed() && i < seeds.size(); j++, i++) {
    MWCseeds.push_back(seeds[i]);
  }
  for (int j = 0; j < shr3_->nseed() && i < seeds.size(); j++, i++) {
    SHR3seeds.push_back(seeds[i]);
  }
  for (int j = 0; j < cong_->nseed() && i < seeds.size(); j++, i++) {
    CONGseeds.push_back(seeds[i]);
  }

  mwc_->vec_reseed(MWCseeds);
  shr3_->vec_reseed(SHR3seeds);
  cong_->vec_reseed(CONGseeds);
}

int
SimpleCombo::nseed()
{
  return mwc_->nseed() + shr3_->nseed() + cong_->nseed();
}

////////////////////////////////////////////////////////////////////
// Table256

Table256::Table256()
{
  seeder_ = SimpleCombo::construct();
}

Table256::~Table256()
{
  delete seeder_;
}

void
Table256::vec_reseed(const std::vector<rngint_t> &seeds)
{
  c = 0;
  seeder_->vec_reseed(seeds);
  for (int i = 0; i < 256; i++) {
    t[i] = seeder_->value();
  }
}

int
Table256::nseed()
{
  return seeder_->nseed();
}

////////////////////////////////////////////////////////////////////
// LFIB4

LFIB4::LFIB4()
{
}

LFIB4*
LFIB4::construct()
{
  LFIB4* result(new LFIB4);
  result->reseed();
  return result;
}

LFIB4*
LFIB4::construct(const std::vector<rngint_t> &seeds)
{
  LFIB4* result(new LFIB4);
  result->vec_reseed(seeds);
  return result;
}

LFIB4::~LFIB4()
{
}

void
LFIB4::vec_reseed(const std::vector<rngint_t> &seeds)
{
  Table256::vec_reseed(seeds);
}

int
LFIB4::nseed()
{
  return Table256::nseed();
}

////////////////////////////////////////////////////////////////////
// SWB

SWB::SWB()
{
}

SWB*
SWB::construct()
{
  SWB* result(new SWB);
  result->reseed();
  return result;
}

SWB*
SWB::construct(const std::vector<rngint_t> &seeds)
{
  SWB* result(new SWB);
  result->vec_reseed(seeds);
  return result;
}

SWB::~SWB()
{
}

void
SWB::vec_reseed(const std::vector<rngint_t> &seeds)
{
  x = 0;
  y = 0;
  Table256::vec_reseed(seeds);
}

int
SWB::nseed()
{
  return Table256::nseed();
}

////////////////////////////////////////////////////////////////////
// Combo

Combo::Combo()
{
  swb_ = SWB::construct();
  simplecombo_ = SimpleCombo::construct();
}

Combo*
Combo::construct()
{
  Combo* result(new Combo);
  result->reseed();
  return result;
}

Combo*
Combo::construct(const std::vector<rngint_t> &seeds)
{
  Combo* result(new Combo);
  result->vec_reseed(seeds);
  return result;
}

Combo::~Combo()
{
  delete swb_;
  delete simplecombo_;
}

void
Combo::vec_reseed(const std::vector<rngint_t> &seeds)
{
  std::vector<rngint_t> SWBseeds, SimpleComboseeds;

  unsigned int i = 0;
  for (int j = 0; j < swb_->nseed() && i < seeds.size(); j++, i++) {
    SWBseeds.push_back(seeds[i]);
  }
  for (int j = 0; j < simplecombo_->nseed() && i < seeds.size(); j++, i++) {
    SimpleComboseeds.push_back(seeds[i]);
  }

  swb_->vec_reseed(SWBseeds);
  simplecombo_->vec_reseed(SimpleComboseeds);

  // This advances the simplecombo_ RNG to mimic the behavior of
  // the original RNG code, which used the same simplecombo
  // as the one used to seed the table.
  for (int i = 0; i < 256; i++) {
    simplecombo_->value();
  }
}

rngint_t
SHR3::value()
{
  //return (jsr = (jsr = (jsr = jsr ^ (jsr << 17)) ^ (jsr >> 13)) ^ (jsr << 5));
  jsr = jsr ^ (jsr << 17);
  jsr = jsr ^ (jsr >> 13);
  jsr = jsr ^ (jsr << 5);
  return jsr;
}

rngint_t
SWB::value()
{
  unsigned char i1, i2, i3;
  i1 = c + 237;
  i2 = c + 15;
  i3 = ++c;
  //rngint_t ret = (t[i1] = (x = t[i2]) - (y = t[i3] + (x < y)));
  x = t[i2];
  int shifter = x < y;
  y = t[i3];
  rngint_t ret = t[i1] = x - (y + (shifter));
  return ret;
  t[i1] = x - (y + (x<y));
  x = t[i2];
  y = t[i3];
  return t[i1];
}

int
Combo::nseed()
{
  return swb_->nseed() + simplecombo_->nseed();
}

#define TWO_PI 6.28318530718
#define ONE_MILLION 1000000
NormalDistribution::NormalDistribution(double mean, double stdev, double maxZ, rngint_t seed)
  : mean_(mean), stdev_(stdev), maxZ_(maxZ), reuse_(false), Y_(0)
{
  if (seed == 0){
    seed = time(NULL);
  }
  rand_ = SHR3::construct(seed);
}

NormalDistribution::~NormalDistribution()
{
  delete rand_;
}

double
NormalDistribution::value()
{
  if (reuse_) {
    reuse_ = false;
    return Y_;
  }

  int microparts = rand_->value_in_range(ONE_MILLION);
  double U = (double) microparts / (double) ONE_MILLION;

  microparts = rand_->value_in_range(ONE_MILLION);
  double V = (double) microparts / (double) ONE_MILLION;

  double cos2pv = cos(TWO_PI * V);
  double sin2pv = sin(TWO_PI * V);
  double prefac = sqrt(-2.0 * log(U));
  double X = prefac * cos2pv;
  double Y = prefac * sin2pv;

  if (X > maxZ_) {
    X = maxZ_;
  }
  else if (X < -maxZ_) {
    X = -maxZ_;
  }
  X = X * stdev_ + mean_;

  if (Y > maxZ_) {
    Y = maxZ_;
  }
  else if (Y < -maxZ_) {
    Y = -maxZ_;
  }
  Y = Y * stdev_ + mean_;

  reuse_ = true;
  Y_ = Y;

  return X;
}


double
ExponentialDistribution::value()
{
  int microparts = rand_->value_in_range(ONE_MILLION);
  double U = (double) microparts / (double) ONE_MILLION;
  double x = -log(U) / lambda_;
  return x;
}

}