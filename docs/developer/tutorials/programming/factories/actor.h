#include <sprockit/factories/factory.h>

namespace sstmac {
namespace tutorial {

class actor
{

 public:
  virtual void
  act() = 0;

  virtual ~actor() {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual actor*
  clone() const = 0;

 protected:
  std::string biggest_fan_;
};

DeclareFactory(actor);

}
}

