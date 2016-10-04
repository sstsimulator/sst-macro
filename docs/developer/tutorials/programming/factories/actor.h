#include <sprockit/factories/factory.h>
#include <sprockit/printable.h>

namespace sstmac {
namespace tutorial {

class actor : public sprockit::printable
{

 public:
  actor(sprockit::sim_parameters* params);

  virtual void
  act() = 0;

  virtual ~actor() {}

 protected:
  std::string biggest_fan_;
};

DeclareFactory(actor);

}
}

