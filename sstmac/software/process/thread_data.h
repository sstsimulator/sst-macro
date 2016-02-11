#ifndef THREAD_DATA_H
#define THREAD_DATA_H

#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/threading/threading_interface_fwd.h>
#include <utility>

namespace sstmac {
namespace sw {

// We need to know the thread associated with each threading_interface
// so current thread pointer can be maintained
// (used for threadstack_ in operatingsystem and block_ in key )
typedef std::pair<threading_interface*, thread*> thread_data_t;

/**
 * Base class for tasks that can be cooperative scheduled using threadcontext.
 */

struct threadinfo {
  thread* thethread;
};

}
}

#endif // THREAD_DATA_H
