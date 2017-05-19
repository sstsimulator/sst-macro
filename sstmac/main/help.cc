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

#include <sprockit/output.h>
#include <cstring>

void
print_help(int argc, char **argv)
{
  char *name = argv[0];
  char *tail = name + strlen(name);
  while (tail != name && *tail != '/') {
    --tail;
  }
  if (tail != name) {
    ++tail;
  }

  cout0 << "usage: " << tail << " -f <config file> [options] \n" << " \n"
            << "Options are: \n" << "\t[(--help|-h)]                          \n"
            << "\t[(--print-apps|-P)]                    \n"
            << "\t[(--debug|-d)             <value> ]    \n"
            << "\t[(--configfile|-f)        <value> ]    \n"
            << "\t[(--param|-p)       <key>=<value> ]    \n"
            << "\t[(--stoptime|-t)          <value> ]    \n"
            << "\t[(--mode|-m)              <value> ]    \n"
            << "\t[(--include|-i)           <value> ]    \n"
            << "\t[(--runnumber|-r)         <value> ]    \n"
            << "\t[(--cpu-affinity|-c)      <value>,<value>,... ]    \n"
            << "\n"

            << "Configuration file is not optional. See parameters.ini for \n"
            << "a list of parameters. \n" << "\n"
            << "--param (-p) is used to override any parameters in the \n"
            << "configuration file, for example: \n" << "\t-p network_name=buffer \n"
            << "\n--include (-i) is used to include a pre-made parameter file \n"
            << "from the configurations folder into your parameter set\n" << "\n"
            << "--stoptime (-t) stops the simulation at the given time, \n"
            << "which is useful for debugging \n" << "\n"
            << "--mode (-m) can be used to open the debugger by saying -m debug, \n"
            << "and can turn off printing the simulation at the end with -m notime \n"
            << "\n--cpu-affinity takes a comma separated list of processor affinities\n"
            << "with size equal to the number of PDES tasks per node\n"
            << "\n" << "Valid arguments to --debug (-d) are strings of the form \n"
            << "\"<(debug|stats)> (name1) | (name2) | ... \" \n"
            << "\t- examples: \n"
            << "\t\t- \"<debug> mpi\" turns on all mpi output \n"
            << "\t\t- \"<debug> app | mpicollective\" turns on all \n"
            << "\t\tskeleton and mpicollective output \n"
            << "\t\t- \"<stats> all\" turns on all stat collection \n"
            << "\t- some current useful debug tags are: \n"
            << "\t\tsoftware: app mpi mpiapi sstmac_mpi mpicollective mpi_queue \n"
            << "\t\thardware: node network nic  \n"
            << "\t\tsystem: launch allocation indexing \n"
            << "\t- current stat tags are: \n"
            << "\t\tall spyplot congestion network \n";

}