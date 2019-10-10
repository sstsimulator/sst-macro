
#ifndef sstmac_sw_process_ftq_scope_h
#define sstmac_sw_process_ftq_scope_h

#include <sstmac/common/stats/ftq_tag.h>
#include <sstmac/software/process/thread_fwd.h>

//Always include the following classes with both standalone/sst-core
namespace sstmac {
namespace sw {

// Ensures an ftq_tag is used for the life of this object
class FTQScope {
 public:
  FTQScope(Thread* thr, const FTQTag& tag);
  ~FTQScope();

 private:
  FTQTag prev_tag_;
  Thread* thr_;
};

}
}

#endif

