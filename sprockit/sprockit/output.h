#ifndef sprockit_common_OUTPUT_H
#define sprockit_common_OUTPUT_H

#include <iostream>

#define cout0 ::sprockit::output::out0()
#define coutn ::sprockit::output::outn()
#define cerr0 ::sprockit::output::err0()
#define cerrn ::sprockit::output::errn()


namespace sprockit {

class output
{
 public:
  static std::ostream&
  out0() {
    return (*out0_);
  }

  static std::ostream&
  outn() {
    return (*outn_);
  }

  static std::ostream&
  err0() {
    return (*err0_);
  }

  static std::ostream&
  errn() {
    return (*errn_);
  }

  static void
  init_out0(std::ostream* out0){
    out0_ = out0;
  }

  static void
  init_outn(std::ostream* outn){
    outn_ = outn;
  }

  static void
  init_err0(std::ostream* err0){
    err0_ = err0;
  }

  static void
  init_errn(std::ostream* errn){
    errn_ = errn;
  }

 protected:
  static std::ostream* out0_;
  static std::ostream* outn_;
  static std::ostream* err0_;
  static std::ostream* errn_;

};

}

#endif // OUTPUT_H
