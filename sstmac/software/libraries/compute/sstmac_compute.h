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
#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_SSTMAC_COMPUTE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_SSTMAC_COMPUTE_H_INCLUDED

#ifdef __cplusplus
extern "C"
{
#endif

void
SSTMAC_compute(double seconds);

void
SSTMAC_compute_block_read(long long bytes);


// ----------- compute loop functions ------------- //
void
SSTMAC_compute_loop(long long from, long long to, long long numlines);

void
SSTMAC_compute_loop2(long long from1, long long to1, long long from2,
                     long long to2, long long numlines);

void
SSTMAC_compute_loop3(long long from1, long long to1, long long from2,
                     long long to2, long long from3, long long to3,
                     long long numlines);

void
SSTMAC_compute_loop4(long long from1, long long to1, long long from2,
                     long long to2, long long from3, long long to3,
                     long long from4, long long to4, long long numlines);

// --------------------------------------------------- //

#ifdef __cplusplus
}
#endif

#endif

