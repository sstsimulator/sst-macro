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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPISTATUS_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPISTATUS_H_INCLUDED

struct MPI_Status
{
  int source;
  int tag;
  int count;
  int bytes_received;
};

#endif

