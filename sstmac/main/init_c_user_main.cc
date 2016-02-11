/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2010 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */


extern "C" int user_skeleton_main(int,char**);

typedef int (*main_fxn)(int, char**);
namespace sstmac { namespace sw {
extern main_fxn user_skeleton_main;
}}

static int init_fxn()
{
  sstmac::sw::user_skeleton_main = &user_skeleton_main;
  return 42;
}

extern "C" int sstmac_c_linkage_integer = init_fxn();

