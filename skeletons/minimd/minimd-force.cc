#include "minimd-force.h"

#include "minimd-atom.h"
#include "minimd-neighbor.h"

namespace mini
{

  void
  minimd::force::compute(const sprockit::refcount_ptr<atom>& a,
      const sprockit::refcount_ptr<neighbor>& n)
  {
    execution_->compute("Force::compute", a, n, 0);
  }

}
