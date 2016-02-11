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

#include <sstmac/software/threading/context_util.h>

namespace sstmac {
namespace sw {

// Intermediary to get around the brain-damaged prototype for makecontext.
void context_springboard(int func_ptr_a, int func_ptr_b,
                         int arg_ptr_a,  int arg_ptr_b)
{
  funcptr fptr(func_ptr_a, func_ptr_b);
  voidptr vptr(arg_ptr_a, arg_ptr_b);
  fptr.call(vptr);
}

}
} // end of namespace sstmac

