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

#include <stdio.h>
#include <string>
#include <cstdarg>

#include <sprockit/spkt_config.h>

namespace sprockit {

  std::string
  printf(const char *fmt, ...)
  {
    char tmpbuf[512];

    va_list args;
    va_start(args, fmt);
    vsprintf(tmpbuf, fmt, args);
    std::string strobj = tmpbuf;
    va_end(args);

    return strobj;
  }

}

