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

#ifdef __cplusplus
struct MPI_Status {
#else
typedef struct {
#endif
  int MPI_SOURCE;
  int MPI_TAG;
  int MPI_ERROR;
  int count;
  int bytes_received;
#ifdef __cplusplus
};
#else
} MPI_Status;
#endif

#endif

