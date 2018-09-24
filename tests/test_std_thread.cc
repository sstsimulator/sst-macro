/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#include <sstmac/replacements/thread>
#include <sstmac/replacements/mutex>
#include <sstmac/skeleton.h>
#include <sstmac/compute.h>

using namespace sstmac;
using namespace sstmac::sw;

extern "C" int ubuntu_cant_name_mangle() { return 0; }

void fxn1(const char* arg){
  std::cout << arg << std::endl;
}

void fxn2(const std::string& arg){
  std::cout << arg << std::endl;
}

void fxn3(int a, int b){
  int sum = a + b;
  std::cout << "Sum = " << sum << std::endl;
}

void thrash(std::mutex* mtx)
{
  for (int i=0; i < 3; ++i){
    std::cout << "Locking T=" << i << std::endl;
    mtx->lock();
    mtx->unlock();
  }
}


#define sstmac_app_name test_std_thread


int USER_MAIN(int argc, char** argv)
{
  std::thread t1(fxn1, "I got my head checked");
  std::thread t2(fxn2, "By a jumbo jet");
  t1.join();
  std::thread t3(fxn3, 3, 5);
  t2.join();
  t3.join();

  //now test some mutexes
  std::mutex mtx;
  std::thread t4(thrash, &mtx);
  std::thread t5(thrash, &mtx);
  std::thread t6(thrash, &mtx);

  t4.join();
  t5.join();
  t6.join();

  return 0;
}

