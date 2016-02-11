/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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
                   "failed to a find a dumpi file starting from the fileroot %s",
                   fileroot_.c_str());
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


