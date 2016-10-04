/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */


enum TEST_MODE
{

  TEST_CONFLICTS = 0,
  TEST_OPERATORS = 1,
  TEST_FUNCTIONS = 2,
  TEST_ARRAYS = 3,
  TEST_STRUCTS = 4,
  TEST_MODE_END = 5

};

//-------- feature tests---------//
#include "tests/test_conflicts.cc"
#include "tests/test_operators.cc"
#include "tests/test_functions.cc"
#include "tests/test_arrays.cc"
#include "tests/test_structs.cc"

#include <sstmac/util.h>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>

using namespace sstmac;

int testmode_ = -1;
bool justprint = false;
bool printnumtests = false;

std::string
print_test_name(int testmode);

const char* valid_keywords[] = {
"testsuite_testmode",
"testsuite_print",
"testsuite_numtests" };
sprockit::StaticKeywordRegister reg(3, valid_keywords);

#define sstmac_app_name apitest

int
USER_MAIN(int argc, char *argv[])
{
  sprockit::sim_parameters* params = get_params();
  testmode_ = params->get_int_param("testsuite_testmode");
  justprint = params->get_optional_bool_param("testsuite_print", false);
  printnumtests = params->get_optional_bool_param("testsuite_numtests", false);

  if (justprint)
  {
    std::cout << "name: " << print_test_name(testmode_) << "\n";
    return 0;
  }
  else if (printnumtests)
  {
    std::cout << (TEST_MODE_END - 1) << "\n";
    return 0;
  }

  switch (testmode_)
  {

  case TEST_CONFLICTS:
    test_conflicts::test_conflicts(argc, argv);
    break;

  case TEST_OPERATORS:
    test_operators::test_operators(argc, argv);
    break;

  case TEST_FUNCTIONS:
    test_functions::test_functions(argc, argv);
    break;

  case TEST_ARRAYS:
    test_arrays::test_arrays(argc, argv);
    break;

  case TEST_STRUCTS:
    test_structs::test_structs(argc, argv);
    break;

  default:
    spkt_throw_printf(sprockit::spkt_error, "testglobals: unknown test mode %d", testmode_);
    return 1;
  }
  return 0;
}

std::string
print_test_name(int testmode)
{
  switch (testmode)
  {
  case TEST_CONFLICTS:
    return "TEST_CONFLICTS";

  case TEST_OPERATORS:
    return "TEST_OPERATORS";

  case TEST_FUNCTIONS:
    return "TEST_FUNCTIONS";

  case TEST_ARRAYS:
    return "TEST_ARRAYS";

  case TEST_STRUCTS:
    return "TEST_STRUCTS";

  default:
    return "not a valid test mode";

  }
}

