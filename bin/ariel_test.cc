/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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
#include <sst/elements/ariel/ariel_shmem.h>
#include <iostream>
#include <unistd.h>

using namespace SST::ArielComponent;

int main(int argc, char** argv)
{
  for (int i=1; i < argc; ++i){
    std::cout << "Got arg: " << argv[i] << std::endl;
  }
  std::string name(argv[1]);
  int initial_pause = 3;
  std::cout << "Starting app in";
  for (int i=initial_pause; i > 0; --i){
    std::cout << " ..." << i;
    std::cout.flush();
    sleep(1);
  }
  std::cout << std::endl;
  auto* tunnel = new ArielTunnel(name);

  static const int numOps = 1000;
  for (int op=0; op < numOps; ++op){
    ArielCommand ac;
    ac.command = ARIEL_START_INSTRUCTION;
    tunnel->writeMessage(0, ac);

    ac.command = ARIEL_PERFORM_READ;
    ac.instPtr = 0;
    ac.inst.addr = 0x4000000;
    ac.inst.size = 4096;
    ac.inst.instClass = 0;
    ac.inst.simdElemCount = 0;
    tunnel->writeMessage(0, ac);

    ac.command = ARIEL_END_INSTRUCTION;
    tunnel->writeMessage(0, ac);
  }
  ArielCommand ac;
  ac.command = ARIEL_PERFORM_EXIT;
  tunnel->writeMessage(0, ac);

  return 0;
}

