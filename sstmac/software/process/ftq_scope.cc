#include <sstmac/software/process/ftq_scope.h>
#include <sstmac/software/process/thread.h>

namespace sstmac {
namespace sw {

// ftq_scope member functions
FTQScope::FTQScope(Thread* thr, const FTQTag& tag) :
  prev_tag_(thr->tag()), thr_(thr)
{
  if (tag.level() >= thr->tag().level()){
    thr->setTag(tag);
  }
}

FTQScope::~FTQScope()
{
  thr_->setTag(prev_tag_);
}

}
}


