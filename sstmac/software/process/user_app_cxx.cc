/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/software/process/user_app_cxx.h>
#include <sstmac/software/process/graphviz.h>

namespace sstmac {
namespace sw {

SpktRegister("c++ | CXX | C++ | cxx | user_app_cxx | user_mpiapp_cxx | user_shmemapp_cxx | user_upcapp_cxx",
            app, user_app_cxx,
            "default application for linking to external C++ skeleton apps");


} //end namespace sw
} //end namespace sstmac







