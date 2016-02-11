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

#ifndef SSTMAC_SOFTWARE_THREADING_THREADING_INTERFACE_H_INCLUDED
#define SSTMAC_SOFTWARE_THREADING_THREADING_INTERFACE_H_INCLUDED

#include <sstmac/common/sstmac_config.h>
#include <errno.h>
#include <cstring>
#include <iostream>

namespace sstmac {
namespace sw {


class threading_interface
{
 public:
  virtual ~threading_interface() {}

  virtual threading_interface*
  copy() = 0;

  virtual void
  init_context() = 0;

  virtual void
  destroy_context() = 0;

  /// Start a new context.
  virtual void
  start_context(int physical_thread_id,
                void *stack, size_t stacksize, void
                (*func)(void*), void *args, threading_interface *yield_to) = 0;

  /// Swap context.
  virtual void
  swap_context(threading_interface* to) = 0;

  /// This is called when we have completed running the thread. It is
  /// called in the from context.
  virtual void
  complete_context(threading_interface* to) = 0;
};
}
} // end of namespace sstmac
#endif

