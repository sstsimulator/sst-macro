/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/common/c_params.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/common/sstmac_env.h>

namespace sstmac {

/* extern "C" bool
 get_bool_param(const char* str)
 {
   std::string p(str);
   return sstmac::env::params->get_bool_param(str);
 }

 extern "C" bool
 get_optional_bool_param(const char* str, bool val)
 {
   std::string p(str);
   return sstmac::env::params->get_optional_bool_param(str, val);
 }*/

extern "C" int
get_int_param(char* str)
{
  std::string p(str);
  return sstmac::env::params->get_int_param(str);
}

extern "C" int
get_optional_int_param(char* str, int val)
{
  std::string p(str);
  return sstmac::env::params->get_optional_int_param(str, val);
}

extern "C" long
get_long_param(char* str)
{
  std::string p(str);
  return sstmac::env::params->get_long_param(str);
}

extern "C" long
get_optional_long_param(char* str, long val)
{
  std::string p(str);
  return sstmac::env::params->get_optional_long_param(str, val);
}

extern "C" double
get_double_param(char* str)
{
  std::string p(str);
  return sstmac::env::params->get_double_param(str);
}

extern "C" double
get_optional_double_param(char* str, double val)
{
  std::string p(str);
  return sstmac::env::params->get_optional_double_param(str, val);
}

extern "C" const char*
get_param(char* str)
{
  std::string p(str);
  return sstmac::env::params->get_param(str).c_str();
}

extern "C" const char*
get_optional_param(char* str, char* val)
{
  std::string p(str);
  std::string sval(val);
  return sstmac::env::params->get_optional_param(str, sval).c_str();
}

extern "C" bool
get_bool_param(char* str)
{
  std::string p(str);
  return sstmac::env::params->get_bool_param(str);
}

extern "C" bool
get_optional_bool_param(char* str, bool val)
{
  std::string p(str);
  return sstmac::env::params->get_optional_bool_param(str, val);
}

}

