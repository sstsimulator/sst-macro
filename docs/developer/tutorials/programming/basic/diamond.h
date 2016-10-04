#include "gem.h"
#include "mineral.h"

namespace sstmac {
namespace tutorial {

class diamond :
  public gem,
  public mineral
{
  /** Public typedefs */
 public:
  typedef sprockit::refcount_ptr<diamond> ptr;
  typedef sprockit::refcount_ptr<const diamond> const_ptr;

  /** Public functions and constructors */
 public:
  diamond(num_carats)
    : num_carats_(num_carats) {
  }

  /** Fill out ptr_type virtual interface */
  std::string
  to_string() const override {
    return "diamond";
  }

  int
  value() const {
    return num_carats_ * 100;
  }

  std::string
  structure() const {
    return "tetrahedral carbon";
  }

  /** Member variables */
 protected:
  int num_carats_;
};

}
}

