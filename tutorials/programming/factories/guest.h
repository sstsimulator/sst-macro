#include "actor.h"

namespace sstmac {
namespace tutorial {

class christopher_guest :
  public actor
{

 public:
  std::string
  to_string() const {
    return "count rugen";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual actor*
  clone() const;

  virtual void
  act();

 protected:
  void
  clone_into(christopher_guest* cln) const;

 protected:
  int num_fingers_;

};

}
}

