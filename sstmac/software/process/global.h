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

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_H_INCLUDED

#include <sstream>

#include <sprockit/unordered.h>
#include <sstmac/software/process/global_builtin.h>
#include <sstmac/software/process/global_builtin_array.h>
#include <sstmac/software/process/global_ptr.h>
#include <sstmac/software/process/global_ptr_array.h>
#include <sstmac/software/process/global_struct.h>

typedef sstmac::sw::sstmac_global_builtin<double*> global_double_ptr;
typedef sstmac::sw::sstmac_global_builtin<double> global_double;
typedef sstmac::sw::sstmac_global_builtin<char*> global_char_ptr;
typedef sstmac::sw::sstmac_global_builtin<char> global_char;
typedef sstmac::sw::sstmac_global_builtin<long*> global_long_ptr;
typedef sstmac::sw::sstmac_global_builtin<long> global_long;
typedef sstmac::sw::sstmac_global_builtin<int*> global_int_ptr;

typedef sstmac::sw::sstmac_global_builtin<int> global_int;
typedef sstmac::sw::sstmac_global_builtin<long long> global_long_long;
typedef sstmac::sw::sstmac_global_builtin<bool*> global_bool_ptr;
typedef sstmac::sw::sstmac_global_builtin<bool> global_bool;

template<typename T, int N>
struct global_arr : public sstmac::sw::sstmac_global_builtin_arr<T, N> {

};


template<typename T, int N>
struct global_ptr_arr : public sstmac::sw::sstmac_global_builtin_arr<T*, N> {

};

template<typename T>
void
delete_global(const sstmac::sw::sstmac_global_builtin<T*> &x)
{
  delete x.get_val();
}

template<typename T>
void
delete_global(const sstmac::sw::sstmac_global_struct<T*> &x)
{
  delete x.get_val();
}

#endif

