#include <sprockit/ptr_type.h>

namespace sstmac {
namespace tutorial {
/**
 * @brief The mineral class
 * @class mineral
 */
class mineral :
  virtual public sprockit::ptr_type
{
  /** Public typedefs */
 public:
  typedef sprockit::refcount_ptr<mineral> ptr;
  typedef sprockit::refcount_ptr<const mineral> const_ptr;

  /** Public functions and constructors */
 public:
  virtual std::string
  structure() const = 0;

  virtual ~mineral() {}

};

}
}

