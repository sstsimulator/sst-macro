#ifndef FAILABLE_H
#define FAILABLE_H

#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/hardware/common/connection.h>

namespace sstmac {
  namespace hw {

class failable
{

 public:
  bool
  failed() const {
    return failed_;
  }

  void
  fail(){
    failed_ = true;
  }

 protected:
  failable() :
   failed_(false)
  {
  }

 protected:
  bool failed_;

};

  }
}

#endif // FAILABLE_H
