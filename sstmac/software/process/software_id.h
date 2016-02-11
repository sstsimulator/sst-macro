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

#ifndef SSTMAC_SOFTWARE_PROCESS_software_id_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_software_id_H_INCLUDED

#include <iostream>
#include <sstream>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/thread_id.h>

namespace sstmac {
namespace sw {

/**
 * A wrapper for an appid, taskid pair
 *
 */
struct software_id {
  app_id app_;
  task_id task_;
  thread_id thread_;

  explicit software_id(app_id a, task_id t, thread_id th = thread_id(0))
    : app_(a), task_(t), thread_(th) {}

  software_id() : app_(-1), task_(-1) { }

  std::string to_string() const {
    std::stringstream ss(std::stringstream::in | std::stringstream::out);
    ss << "software_id(" << app_ << ", " << task_ << "," << thread_ << ")";
    return ss.str();
  }
};

inline bool operator==(const software_id &a, const software_id &b)
{
  return (a.app_ == b.app_ && a.task_ == b.task_ && a.thread_ == b.thread_);
}
inline bool operator!=(const software_id &a, const software_id &b)
{
  return (a.app_ != b.app_ || a.task_ != b.task_ || a.thread_ != b.thread_);
}
inline bool operator<(const software_id &a, const software_id &b)
{
  return (a.task_ < b.task_);
}
inline bool operator>(const software_id &a, const software_id &b)
{
  return (a.task_ > b.task_);
}


inline std::ostream& operator<<(std::ostream &os, const software_id &n)
{
  return (os << n.to_string());
}

}
} // end of namespace sstmac

#endif

