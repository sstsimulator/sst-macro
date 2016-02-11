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

#include <sstmac/software/libraries/compute/sstmac_compute.h>


extern "C" void
sstmac_compute_(double& seconds)
{
  SSTMAC_compute(seconds);
}

extern "C" void
sstmac_compute_block_read_(int& bytes)
{
  SSTMAC_compute_block_read(bytes);
}

extern "C" void
sstmac_compute_loop_0_(int& from, int& to, int& numlines)
{
  SSTMAC_compute_loop(from, to, numlines);
}


extern "C" void
sstmac_compute_loop_1_(int& from, int& to, float& numlines)
{
  SSTMAC_compute_loop(from, to, numlines);
}

extern "C" void
sstmac_compute_loop2_0_(int& from1, int& to1, int& from2, int& to2,
                        int& numlines)
{
  SSTMAC_compute_loop2(from1, to1, from2, to2, numlines);
}

extern "C" void
sstmac_compute_loop2_1_(int& from1, int& to1, int& from2, int& to2,
                        float& numlines)
{
  SSTMAC_compute_loop2(from1, to1, from2, to2, numlines);
}

extern "C" void
sstmac_compute_loop3_0_(int& from1, int& to1, int& from2, int& to2, int& from3,
                        int& to3, int& numlines)
{
  SSTMAC_compute_loop3(from1, to1, from2, to2, from3, to3, numlines);
}

extern "C" void
sstmac_compute_loop3_1_(int& from1, int& to1, int& from2, int& to2, int& from3,
                        int& to3, float& numlines)
{
  SSTMAC_compute_loop3(from1, to1, from2, to2, from3, to3, numlines);
}

extern "C" void
sstmac_compute_loop4_0_(int& from1, int& to1, int& from2, int& to2, int& from3,
                        int& to3, int& from4, int& to4, int& numlines)
{
  SSTMAC_compute_loop4(from1, to1, from2, to2, from3, to3, from4, to4, numlines);
}

extern "C" void
sstmac_compute_loop4_1_(int& from1, int& to1, int& from2, int& to2, int& from3,
                        int& to3, int& from4, int& to4, float& numlines)
{
  SSTMAC_compute_loop4(from1, to1, from2, to2, from3, to3, from4, to4, numlines);
}



