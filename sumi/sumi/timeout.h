#ifndef sumi_api_TIMEOUT_H
#define sumi_api_TIMEOUT_H

#include <sumi/message.h>

namespace sumi {

/**
 * @class timeout_function
 * Abstract class for time-outs invoked by #pinger
 * Timeout action can be anything
 */
class timeout_function
{
 public:
  virtual void
  time_out(int partner) = 0;

  virtual ~timeout_function(){}

};

}

#endif // TIMEOUT_H
