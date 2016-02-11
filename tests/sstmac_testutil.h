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

#ifndef _SSTMAC_TESTUTIL_H
#define _SSTMAC_TESTUTIL_H

#include <sstmac/common/driver_util.h>

#include <limits.h>

#include <gtest/gtest.h>

using sstmac::timestamp;

inline bool
timestamp_near(const timestamp &t, timestamp ttest, double eps=-1.0) {
  double diff = fabs(t.sec() - ttest.sec());
  if (eps < 0.0) {
    eps = 10.0*t.sec()*std::numeric_limits<double>::epsilon();
  }
  if (diff > eps) return false;
  return true;
}





#endif // _SSTMAC_TESTUTIL_H
