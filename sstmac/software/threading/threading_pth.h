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

#ifndef SSTMAC_SOFTWARE_THREADING_THREADING_PTH_H_INCLUDED
#define SSTMAC_SOFTWARE_THREADING_THREADING_PTH_H_INCLUDED

#include <sstmac/software/threading/threading_interface.h>

#ifdef SSTMAC_HAVE_PTH
#  ifdef SSTMAC_HAVE_PTH_CUSTOM
#    include <pth-sstmac.h>
#  else // SSTMAC_HAVE_PTH_CUSTOM
#    include <pth.h>
#  endif // SSTMAC_HAVE_PTH_CUSTOM
#endif

namespace sstmac {
namespace sw {

#ifdef SSTMAC_HAVE_PTH

class threading_pth : public threading_interface
{
  typedef pth_uctx_t threadcontext_t;

 private:
  threadcontext_t context_;

 public:

  virtual
  threading_interface* copy() {
    return new threading_pth();
  }

  // Initializing a context.
  virtual void
  init_context();

  virtual void
  destroy_context();

  /// Start a new context.
  virtual void
  start_context(int physical_thread_id, void *stack, size_t stacksize, void
                (*func)(void*), void *args, threading_interface *yield_to);

  /// Swap context.
  virtual void
  swap_context(threading_interface *to);

  /// This is called when we have completed running the thread. It is
  /// called in the from context.
  virtual void
  complete_context( threading_interface *to) {
    swap_context(to);
  }

};

#endif
}
} // end of namespace sstmac
#endif

