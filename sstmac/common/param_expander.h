#ifndef PARAM_EXPANDER_H
#define PARAM_EXPANDER_H

#include <sprockit/factories/factory.h>
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {

class param_expander
{
  DeclareFactory(param_expander)
 public:
  virtual ~param_expander(){}

  virtual void expand(sprockit::sim_parameters* params) = 0;

};

}

#endif // PARAM_EXPANDER_H
