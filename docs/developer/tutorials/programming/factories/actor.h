#include <sstmac/sst_core/integrated_component.h>
#include <sprockit/printable.h>

namespace sstmac {
namespace tutorial {

class Actor 
{
 public:
  SST_ELI_DECLARE_BASE(Actor)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(SST::Params&)

  Actor(SST::Params& params);

  virtual void act() = 0;

  virtual ~Actor() {}

 protected:
  std::string biggest_fan_;
};


}
}
