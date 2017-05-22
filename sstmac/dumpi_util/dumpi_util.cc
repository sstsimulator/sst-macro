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

#include <sstmac/dumpi_util/dumpi_util.h>
#include <errno.h>
#include <cstring>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

std::string
dumpi_file_name(int rank, std::string fileroot_)
{
  // Figure out our input file name.
  // Try fileroot-"%01d", fileroot-"%02d", ... up to some reasonable length
  // (in the short term that we will probably not have over 10 billion peers)
  FILE *fp = NULL;
  char fname[256];
  fname[255] = '\0';
  const char *fnamefmt = "%%s-%%0%dd.bin";

  char fmt[30];
  fmt[29] = '\0';
  for (int i = 1; i < 10; ++i) {
    snprintf(fmt, 29, fnamefmt, i);
    snprintf(fname, 255, fmt, fileroot_.c_str(), rank);
    fp = fopen(fname, "r");
    if (fp) {
      fclose(fp);
      return std::string(fname);
    }
    else {
      if (errno !=
          ENOENT) { //if the error is anything else than a no such file or directory, crash
        spkt_throw_printf(sprockit::io_error,
            "dumpi_file_name: error opening file %s: %s",
            fname, ::strerror(errno));
      }
    }
  }
  // If we get here, then no file was found.
  spkt_throw_printf(sprockit::io_error,
    "failed to a find a dumpi file for rank %d starting from the fileroot %s",
    rank, fileroot_.c_str());
}

int
getnumprocs(dumpi_meta* dmeta_)
{
  if (dmeta_) {
    return dmeta_->getnumprocs();
  }
  int nrank = 0;
  try {
    while (true) {
      std::string fname = dumpi_file_name(nrank,
                                dmeta_->dirplusfileprefix_);
      nrank++;
    }
  }
  catch (sprockit::io_error&) {
    //ioerror is thrown when no more trace files can be found
  }
  return nrank;
}

}
}

extern "C"
int undumpi_read_stream(void* profile,
      void *callback,
      void *uarg);


static int dumpi_util_ubuntu_linker_is_an_abomination(){
  return undumpi_read_stream(0,0,0);
}