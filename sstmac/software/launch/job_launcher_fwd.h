#ifndef JOB_LAUNCHER_FWD_H
#define JOB_LAUNCHER_FWD_H

#include <sprockit/refcount_ptr.h>

namespace sstmac {
namespace sw {

class job_launcher;
class task_mapping;

typedef ::sprockit::refcount_ptr<task_mapping> task_mapping_ptr;

}
}

#endif // JOB_LAUNCHER_FWD_H

