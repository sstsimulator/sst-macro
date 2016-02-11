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

#ifndef SSTMAC_COMMON_C_PARAMS_H_INCLUDED
#define SSTMAC_COMMON_C_PARAMS_H_INCLUDED

#ifdef __cplusplus
extern "C"
{
#else
#include <stdbool.h>
#endif
//bool get_bool_param(const char* str);

// bool get_optional_bool_param(const char* str, bool val);

int
get_int_param(char* str);

int
get_optional_int_param(char* str, int val);

long
get_long_param(char* str);

long
get_optional_long_param(char* str, long val);

double
get_double_param(char* str);

double
get_optional_double_param(char* str, double val);

const char*
get_param(char* str);

bool
get_bool_param(char* str);

bool
get_optional_bool_param(char* str, bool val);

const char*
get_optional_param(char* str, char* val);
#ifdef __cplusplus
}
#endif

#endif

