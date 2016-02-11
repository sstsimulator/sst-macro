#include "actor.h"

namespace sstmac {
namespace tutorial {

class mandy_patinkin :
  public actor
{

 public:
  std::string
  to_string() const {
    return "inigo montoya";
  }

  static ptr
  construct(sprockit::sim_parameters* params) {
    return new mandy_patinkin;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual actor*
  clone() const;

  virtual void
  act();

 protected:
  void
  clone_into(mandy_patinkin* cln) const;

 protected:
  std::string sword_hand_;

};

}
}


