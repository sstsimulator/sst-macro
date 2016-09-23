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

#ifndef sprockit_common_util_h
#define sprockit_common_util_h

#include <sprockit/spkt_config.h>
#include <sprockit/errors.h>

#if SPKT_HAVE_CPP11
#include <functional>
#include <tuple>
#endif

namespace sprockit {

template <class Out, class In>
Out*
__safe_cast__(const char* objname,
              const char* file,
              int line,
              In* in,
              const char* error_msg = "error")
{
  Out* out = dynamic_cast<Out*>(in);
  if (!out) {
    spkt_abort_printf("%s: failed to cast object at %s:%d",
                     error_msg, file, line);
  }
  return out;
}

/**
 * First entry in VA_ARGS is the obj
 * Second entry is optional being an error msg
*/
#define safe_cast(type,...) \
    ::sprockit::__safe_cast__<type>(#type, __FILE__, __LINE__, __VA_ARGS__)

#define test_cast(type, obj) \
    dynamic_cast<type*>(obj)

#define known_cast(type,...) \
    safe_cast(type, __VA_ARGS__)

#define interface_cast(type,obj) \
    dynamic_cast<type*>(obj)


// Splat a tuple into a function call
//   Most of the time, you should just use std::bind for this sort of thing,
//   but you might need it for something...
#if SPKT_HAVE_CPP11
namespace detail {

template<int...>
struct integer_meta_sequence
{};

template<int N, int... S>
struct meta_sequence_generator
    : meta_sequence_generator<N-1, N-1, S...>
{};

template<int... S>
struct meta_sequence_generator<0, S...>{
    typedef integer_meta_sequence<S...> type;
};

template <typename Function, typename Tuple, int... S>
typename Function::result_type
splat_tuple_impl(
    Function&& func,
    Tuple&& tup,
    const integer_meta_sequence<S...>&
)
{
  return func(std::get<S>(tup)...);
}

} // end namespace detail

template <typename Tuple, typename Return, typename... Args>
Return splat_tuple(std::function<Return(Args...)>&& func, Tuple&& tup)
{
  return detail::splat_tuple_impl(
      func, tup,
      typename detail::meta_sequence_generator<std::tuple_size<Tuple>::value>::type()
  );
}

#endif


// Modal base class
//template <typename BaseOf, typename ModeEnum, ModeEnum mode>
//class ModalMixin {
//  protected:
//    typedef enum { disabled } disabled_t;
//
//    void throw_method_called_in_wrong_mode(
//        const std::string& method_name
//    ) const {
//      spkt_throw_printf(sprockit::illformed_error,
//          "method %s() of modal base class was called not called in %s mode;"
//          " it is only valid when called in this mode",
//          method_name, BaseOf::mode_names[mode]
//      );
//    }
//
//    bool disabled_ = false;
//
//    void check_mode(
//        const std::string& method_name
//    ) {
//      if(disabled_) {
//        throw_method_called_in_wrong_mode(method_name);
//      }
//    }
//
//    BaseOf* this_;
//
//};

namespace modal_mixin {

typedef enum { disabled } disabled_t;

class ModalMixinBase {
  protected:

    ModalMixinBase() : disabled_(false) { }

    ModalMixinBase(bool disabled) : disabled_(disabled) { }

    virtual std::string get_mode_name() const =0;
    virtual std::string get_baseof_name() const =0;

    virtual ~ModalMixinBase() { }

    void throw_method_called_in_wrong_mode(
        const std::string& method_name
    ) const {
      spkt_throw_printf(sprockit::illformed_error,
          "method %s() of %s is not valid in %s mode",
          get_baseof_name().c_str(),
          get_mode_name().c_str()
      );
    }

    bool disabled_;

    void check_mode(
        const std::string& method_name
    ) const {
      if(disabled_) {
        throw_method_called_in_wrong_mode(method_name);
      }
    }

};

} // end namespace modal_mixin

#define ASSERT_CORRECT_MODE check_mode(__PRETTY_FUNCTION__);

} // end namespace sprockit


// Easy delegation to pointer members
#if SPKT_HAVE_CPP11
#include <utility>
#include <cassert>
// TODO non-pointer-member version
#define SPKT_DELEGATE_ALL_OVERLOADS(method_name, member)    \
  template <typename... __MethArgs>                         \
  auto method_name(__MethArgs&&... __in_args)               \
    -> decltype(                                            \
        member->method_name(                                \
          std::forward<__MethArgs>(__in_args)...            \
        )                                                   \
      )                                                     \
  {                                                         \
    assert(member != NULL);                                 \
    return member->method_name(                             \
        std::forward<__MethArgs>(__in_args)...              \
    );                                                      \
  }
#define SPKT_DELEGATE_ALL_CONST_OVERLOADS(method_name, member)    \
  template <typename... __MethArgs>                         \
  auto method_name(__MethArgs&&... __in_args) const         \
    -> decltype(                                            \
        member->method_name(                                \
          std::forward<__MethArgs>(__in_args)...            \
        )                                                   \
      )                                                     \
  {                                                         \
    assert(member != NULL);                                 \
    return member->method_name(                             \
        std::forward<__MethArgs>(__in_args)...              \
    );                                                      \
  }
#else
#define SPKT_DELEGATE_ALL_OVERLOADS(method_name, member)    \
  spkt_static_assert(false, "SPKT_DELEGATE_ALL_OVERLOADS requires c++11")
#endif

#if SPKT_HAVE_CPP11
#include <iterator>
#include <algorithm>
#define spkt_find_if(iterable, param, ...) \
  std::find_if(iterable.begin(), iterable.end(), \
      [&](const typename std::iterator_traits<decltype(iterable.begin())>::value_type& param) -> bool \
        __VA_ARGS__ \
  )
#else
#include <sprockit/preprocessor.h>
#define spkt_find_if(...) \
  spkt_static_assert(false, "spkt_find_if() requires c++11")
#endif


#endif
