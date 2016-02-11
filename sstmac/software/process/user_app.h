#ifndef USER_APP_H
#define USER_APP_H

#include <sstmac/software/process/app.h>

namespace sstmac {
namespace sw {

typedef int (*main_fxn)(int,char**);
extern main_fxn user_skeleton_main;

class user_app : public app
{

 protected:
  user_app(){}

 public:
  /** Destructor - do nothing */
  virtual ~user_app() throw () {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  void
  skeleton_main();

  void
  consume_params(sprockit::sim_parameters *params){}

};

} //end namespace sw
} //end namespace sstmac


#endif // USER_APP_H

