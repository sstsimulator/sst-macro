#ifndef PARAM_EXPANDER_H
#define PARAM_EXPANDER_H

#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {

class param_expander :
  public sprockit::factory_type
{
 public:
  virtual ~param_expander(){}

  virtual void
  expand(sprockit::sim_parameters* params) = 0;

  virtual void
  finalize_init();

};

DeclareFactory(param_expander);

}

#endif // PARAM_EXPANDER_H
