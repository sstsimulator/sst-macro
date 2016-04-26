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

#ifndef SSTMAC_SOFTWARE_API_MPI_TYPE_OPERATOR_H_INCLUDED
#define SSTMAC_SOFTWARE_API_MPI_TYPE_OPERATOR_H_INCLUDED

#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

class mpi_type_operator
{

 public:
  virtual
  ~mpi_type_operator() {
  }

  virtual std::string
  to_string() const {
    return "mpi_type_operator";
  }

  virtual bool
  needs_unpack() const {
    return false;
  }

  //---- arithmetic operations ------ //
  virtual void
  do_add(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_prod(const void* buf1, const void* buf2, void* result, int count) = 0;

  //---- compirson-based operations ------ //
  virtual void
  do_max(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_min(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_maxloc(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_minloc(const void* buf1, const void* buf2, void* result, int count) = 0;

  //---- logical operations ------ //
  virtual void
  do_and(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_or(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_xor(const void* buf1, const void* buf2, void* result, int count) = 0;

  //---- bitwise operations ------ //
  virtual void
  do_band(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_bor(const void* buf1, const void* buf2, void* result, int count) = 0;

  virtual void
  do_bxor(const void* buf1, const void* buf2, void* result, int count) = 0;

  // --- special --- //
  virtual void
  do_replace(const void* buf1, const void* buf2, void* result,
             int count) = 0;

};

class mpi_null_op :
  public mpi_type_operator
{
 public:
  virtual
  ~mpi_null_op() {
  }

  virtual std::string
  to_string() const {
    return "mpi_null_op";
  }

  //---- arithmetic operations ------ //
  virtual void
  do_add(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_prod(const void* buf1, const void* buf2, void* result, int count) {
  }

  //---- compirson-based operations ------ //
  virtual void
  do_max(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_min(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_maxloc(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_minloc(const void* buf1, const void* buf2, void* result, int count) {
  }

  //---- logical operations ------ //
  virtual void
  do_and(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_or(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_xor(const void* buf1, const void* buf2, void* result, int count) {
  }

  //---- bitwise operations ------ //
  virtual void
  do_band(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_bor(const void* buf1, const void* buf2, void* result, int count) {
  }

  virtual void
  do_bxor(const void* buf1, const void* buf2, void* result, int count) {
  }

  // --- special --- //
  virtual void
  do_replace(const void* buf1, const void* buf2, void* result, int count) {
  }

};

template<typename T>
class mpi_prim_op :
  public mpi_type_operator
{
 public:
  virtual
  ~mpi_prim_op() {
  }

  virtual void
  do_add(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;

    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] + cast2[i];
    }
  }

  virtual void
  do_prod(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] * cast2[i];
    }
  }

  //---- compirson-based operations ------ //
  virtual void
  do_max(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = (cast1[i] > cast2[i]) ? cast1[i] : cast2[i];
    }
  }

  virtual void
  do_min(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = (cast1[i] > cast2[i]) ? cast2[i] : cast1[i];
    }

  }

  virtual void
  do_maxloc(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator: maxloc not compatible with primitive type");
  }

  virtual void
  do_minloc(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator: maxloc not compatible with primitive type");
  }

  //---- logical operations ------ //
  virtual void
  do_and(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] && cast2[i];
    }
  }

  virtual void
  do_or(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] || cast2[i];
    }
  }

  virtual void
  do_xor(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = (cast1[i] && !cast2[i]) || (!cast1[i] && cast2[i]);
    }
  }

  // --- special --- //
  virtual void
  do_replace(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast2[i];
    }
  }

  //---- bitwise operations ------ //
  virtual void
  do_band(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator:band :bitwise not compatible with primitive type");
  }

  virtual void
  do_bor(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator:bor :bitwise not compatible with primitive type");
  }

  virtual void
  do_bxor(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator:bxor :bitwise not compatible with primitive type");
  }

};

template<typename T>
class mpi_prim_bit_op :
  public mpi_prim_op<T>
{
  typedef mpi_prim_op<T> parent;

 public:
  virtual
  ~mpi_prim_bit_op() {
  }

  //---- bitwise operations ------ //
  virtual void
  do_band(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] & cast2[i];
    }
  }

  virtual void
  do_bor(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] | cast2[i];
    }
  }

  virtual void
  do_bxor(const void* buf1, const void* buf2, void* result, int count) {
    T* cast1 = (T*) buf1;
    T* cast2 = (T*) buf2;
    T* rescast = (T*) result;
    for (int i = 0; i < count; i++) {
      rescast[i] = cast1[i] ^ cast2[i];
    }
  }

};

template<typename T, typename S>
class mpi_pair_op :
  public mpi_type_operator
{
 public:
  virtual
  ~mpi_pair_op() {
  }

  virtual bool
  needs_unpack() const {
    return true;
  }

  virtual void
  do_add(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };

    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = cast1[i].a + cast2[i].a;
      rescast[i].b = cast1[i].b + cast2[i].b;
    }
  }

  virtual void
  do_prod(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = cast1[i].a * cast2[i].a;
      rescast[i].b = cast1[i].b * cast2[i].b;
    }
  }

  //---- compirson-based operations ------ //
  virtual void
  do_max(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = (cast1[i].a > cast2[i].a) ? cast1[i].a : cast2[i].a;
      rescast[i].b = (cast1[i].b > cast2[i].b) ? cast1[i].b : cast2[i].b;
    }

  }

  virtual void
  do_min(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = (cast1[i].a > cast2[i].a) ? cast2[i].a : cast1[i].a;
      rescast[i].b = (cast1[i].b > cast2[i].b) ? cast2[i].b : cast1[i].b;
    }

  }

  virtual void
  do_maxloc(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      if (cast1[i].a > cast2[i].a) {
        rescast[i].a = cast1[i].a;
        rescast[i].b = cast1[i].b;
      }
      else {
        rescast[i].a = cast2[i].a;
        rescast[i].b = cast2[i].b;
      }
    }
  }

  virtual void
  do_minloc(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      if (cast1[i].a < cast2[i].a) {
        rescast[i].a = cast1[i].a;
        rescast[i].b = cast1[i].b;
      }
      else {
        rescast[i].a = cast2[i].a;
        rescast[i].b = cast2[i].b;
      }
    }
  }

  //---- logical operations ------ //
  virtual void
  do_and(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = cast1[i].a && cast2[i].a;
      rescast[i].b = cast1[i].b && cast2[i].b;
    }
  }

  virtual void
  do_or(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = cast1[i].a || cast2[i].a;
      rescast[i].b = cast1[i].b || cast2[i].b;
    }
  }

  virtual void
  do_xor(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = (cast1[i].a && !cast2[i].a) || (!cast1[i].a
                     && cast2[i].a);
      rescast[i].b = (cast1[i].b && !cast2[i].b) || (!cast1[i].b
                     && cast2[i].b);
    }
  }

  // --- special --- //
  virtual void
  do_replace(const void* buf1, const void* buf2, void* result, int count) {
    struct mytype {
      T a;
      S b;
    };
    mytype* cast1 = (mytype*) buf1;
    mytype* cast2 = (mytype*) buf2;
    mytype* rescast = (mytype*) result;

    for (int i = 0; i < count; i++) {
      rescast[i].a = cast2[i].a;
      rescast[i].b = cast2[i].b;
    }
  }

  //---- bitwise operations ------ //
  virtual void
  do_band(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator:band :bitwise not compatible with pair type");
  }

  virtual void
  do_bor(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator:bor :bitwise not compatible with pair type");
  }

  virtual void
  do_bxor(const void* buf1, const void* buf2, void* result, int count) {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi type operator:bxor :bitwise not compatible with pair type");
  }

};

}

}

#endif

