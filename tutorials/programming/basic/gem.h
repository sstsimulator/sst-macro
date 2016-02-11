#include <sstmac/common/ptr_type.h>

namespace sstmac {
namespace tutorial {
/**
 * @brief The gem class
 * @class gem
 */
class gem :
  virtual public sprockit::ptr_type
{
  /** Public typedefs */
 public:
  typedef sprockit::refcount_ptr<gem> ptr;
  typedef sprockit::refcount_ptr<const gem> const_ptr;

  /** Public functions and constructors */
 public:
  virtual int
  value() const = 0;

  virtual ~gem() {}

};

}
}

