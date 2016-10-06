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

#ifndef SPROCKIT_COMMON_FACTORIES_FACTORY_H_INCLUDED
#define SPROCKIT_COMMON_FACTORIES_FACTORY_H_INCLUDED

#include <cstdio>
#include <iostream>
#include <sprockit/errors.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/spkt_config.h>
#include <map>

namespace sprockit {

/**
 *  Object that will actually build classes from the given list of arguments
 */
template <class T, typename... Args>
class SpktBuilder
{
 public:
  virtual T*
  build(sprockit::sim_parameters* params, const Args&... args) = 0;

};

/**
 *  For partial template specialization.  The Factory object is
 *  never directly specified and will be determined automatically from
 *  a child class type and the top-level parent type (see below)
 */
template<class Child, class Factory>
class SpktBuilderImpl
{
};

#include <type_traits>


template <class> struct wrap { typedef void type; };

/**
 * @class call_finalize_init
 * finalize_init is not a required method for any of the classes.
 * This uses SFINAE tricks to call finalize_init on classes that implement
 * the method or avoid calling it on classes that don't implement it
 */
template<typename T, class Enable=void>
struct call_finalize_init : public std::false_type {
public:
  void operator()(T* t){
    //if we compile here, class T does not have a finalize_init
    //do nothing
  }
};

template<typename T>
struct call_finalize_init<T,
  typename wrap<
    decltype(std::declval<T>().finalize_init())
   >::type
> : public std::true_type
{
public:
  void operator()(T* t){
    //if we compile here, class T does have a finalize_init
    //call that function
    t->finalize_init();
  }
};

/**
 *  Class used to query what type of constructor the class has
 *  Mainly used for determining if the class takes a sim_parameters
 *  as the first argument to the constructor
 */
template <class T, class... Args>
struct constructible_from
{
  template <class C>
  static C arg();

  template <typename U>
  static std::true_type test(U*,decltype(U(arg<Args>()...))* = nullptr);

  static std::false_type test(...);

  typedef decltype(test(static_cast<T*>(nullptr))) type;
};

template <typename T, typename Enable, typename... Args>
struct call_constructor_impl {
};

/**
 *  Class used to call particular constructors.  This will get invoked via SFINAE
 *  if the class T does not take a params object as the first parameter in the
 *  constructor
 */
template <typename T, typename... Args>
struct call_constructor_impl<T,
    typename std::enable_if<constructible_from<T,Args...>::type::value>::type,
    Args...>
{
  T* operator()(sprockit::sim_parameters* params, const Args&... args){
    return new T(args...);
  }
};

/**
 *  Class used to call particular constructors.  This will get invoked via SFINAE
 *  if the class T takes a params object as the first parameter in the
 *  constructor
 */
template <typename T, typename... Args>
struct call_constructor_impl<T,
    typename std::enable_if<!constructible_from<T,Args...>::type::value>::type,
    Args...>
{
  T* operator()(sprockit::sim_parameters* params, const Args&... args){
    return new T(params, args...);
  }
};


template <typename T, typename... Args>
class call_constructor : public call_constructor_impl<T, void, Args...> {};


/**
 * @class Factory
 * Object that provides static methods for mapping string names
 * to particular subclasses of T.  The constructor for each subclass U of T
 * must be either U(Args) or it must be U(params,Args) where params is a
 * sim_parameters object
 */
template<class T, typename... Args>
class Factory
{

 public:
  typedef SpktBuilder<T,Args...> builder_t;
  typedef T element_type;

  typedef std::map<std::string, builder_t*> builder_map;
  static builder_map* builder_map_;

  typedef std::map<std::string, std::list<std::string> > alias_map;
  static alias_map* alias_map_;

  static const char* name_;

  /**
   * @brief register_alias Explicitly register an alias name for a
   * factory based on a previous
   * @param oldname The name currently registered or that will be registered
   *                depending on static init order
   * @param newname The new name that can be used for accessing child type
   */
  static void
  register_alias(const std::string& oldname, const std::string& newname){
    if (!builder_map_) {
      builder_map_ = new builder_map;
    }
    if (!alias_map_){
      alias_map_ = new alias_map;
    }

    builder_t* base = (*builder_map_)[oldname];
    if (!base){
      (*alias_map_)[oldname].push_back(newname);
    } else {
      (*builder_map_)[newname] = base;
    }
  }

  static void
  clean_up(){
    //do not iterate the builder map and delete entry
    //each builder_t is a static objec that gets cleaned up automatically

    if (builder_map_) delete builder_map_;
    if (alias_map_) delete alias_map_;

    builder_map_ = 0;
    alias_map_ = 0;
  }

  /**
   * @brief register_name
   * @param name  The string name that will map to the given type
   * @param builder The builder object whose virtual functio returns
   *                the correct child type
   */
  static void
  register_name(const std::string& name, builder_t* builder) {
    if (!builder_map_) {
      builder_map_ = new builder_map;
    }
    if (!alias_map_){
      alias_map_ = new alias_map;
    }
    add_to_map(name, builder, builder_map_, alias_map_);
  }

 private:
  /**
   * @brief add_to_map
   * @param namestr
   * @param desc
   * @param descr_map
   * @param alias_map
   */
  static void
  add_to_map(const std::string& namestr, builder_t* desc,
            std::map<std::string, builder_t*>* builder_map,
            std::map<std::string, std::list<std::string> >* alias_map)
  {
    // The namestr can be a | separate list of valid names, e.g.
    // "torus | hdtorus" to allow either name to map to the correct type
    std::string space = "|";
    std::deque<std::string> tok;
    pst::BasicStringTokenizer::tokenize(namestr, tok, space);

    std::deque<std::string>::iterator it, end = tok.end();

    for (it = tok.begin(); it != end; it++) {
      std::string temp = *it;

      temp = trim_str(temp);

      std::map<std::string, std::list<std::string> >::iterator it = alias_map->find(temp);
      if (it != alias_map->end()){
        std::list<std::string>& alias_list = it->second;
        std::list<std::string>::iterator ait, end = alias_list.end();
        for (ait=alias_list.begin(); ait != end; ++ait){
          (*builder_map_)[*ait] = desc;
        }
      }

      (*builder_map_)[temp] = desc;
    }
  }


  /**
   * @brief _get_value  Helper function that builds the correct child type
   *                    from the corresponding string name
   * @param valname     The string name mapping to a particular child type
   * @param params      The parameters potentially used in the constructor
   * @param args        The required arguments for the constructor
   * @return  A constructed child class corresponding to a given string name
   */
  static T*
  _get_value(const std::string& valname,
             sprockit::sim_parameters* params,
             const Args&... args) {
    if (!builder_map_) {
      spkt_abort_printf(
           "could not find name %s for factory %s. no classes are registered",
           valname.c_str(),
           name_);
    }

    auto it = builder_map_->find(valname), end = builder_map_->end();

    if (it == end) {
      std::cerr << "Valid factories are:" << std::endl;
      for (it = builder_map_->begin(); it != end; ++it) {
        std::cerr << it->first << std::endl;
      }
      params->print_scoped_params(std::cerr);
      spkt_abort_printf("could not find name %s for factory %s",
                       valname.c_str(), name_);
    }

    builder_t* builder = it->second;
    if (!builder) {
      spkt_abort_printf("initialized name %s with null builder for factory %s",
                       valname.c_str(), name_);
    }

    T* p = builder->build(params, args...);
    call_finalize_init<T>()(p);
    return p;
  }

 public:
  /**
   * @brief get_value Return a constructed child class corresponding
   *                  to a given string name
   * @param valname   The string name mapping to a particular child type
   * @param params    The parameters potentially used in the constructor
   * @param args      The required arguments for the constructor
   * @return  A constructed child class corresponding to a given string name
   */
  static T*
  get_value(const std::string& valname,
            sim_parameters* params,
            const Args&... args) {
    return _get_value(valname, params, args...);
  }

  /**
   * @brief get_param Return a constructed child class corresponding
   *                  to a given string name. The string name is not given directly,
   *                  instead being found by params->get_param(param_name)
   * @param param_name   The name of the parameter such that params->get_param(param_name)
   *                     returns the string that will map to the child class
   * @param params    The parameters potentially used in the constructor
   * @param args      The required arguments for the constructor
   * @return  A constructed child class corresponding to a given string name
   */
  static T*
  get_param(const std::string& param_name,
            sim_parameters* params,
            const Args&... args) {
    return _get_value(params->get_param(param_name),
                      params, args...);
  }

  /**
   * @brief get_extra param Return a constructed child class corresponding
   *          to a given string name. The string name is not given directly,
   *          instead being found by params->get_param(param_name).  If no parameter
   *          corresponding to param_name exists, return a nullptr
   * @param param_name   The name of the parameter such that params->get_param(param_name)
   *                     returns the string that will map to the child class
   * @param params    The parameters potentially used in the constructor
   * @param args      The required arguments for the constructor
   * @return  A constructed child class corresponding to a given string name
   */
  static T*
  get_extra_param(const std::string& param_name,
                  sim_parameters* params,
                  const Args&... args) {
    if (params->has_param(param_name)) {
      return get_param(param_name,params);
    }
    else {
      return nullptr;
    }
  }

  static T*
  get_extra_value(const std::string& param_value,
                  sim_parameters* params,
                  const Args&... args){
    if (!builder_map_)
      return nullptr;

    auto it = builder_map_->find(param_value);
    if (it == builder_map_->end()){
      return nullptr;
    } else {
      return _get_value(param_value, params, args...);
    }
  }

  /**
   * @brief get_extra param Return a constructed child class corresponding
   *          to a given string name. The string name is not given directly,
   *          instead being found by params->get_param(param_name).  If no parameter
   *          corresponding to param_name exists, return get_value(defval)
   * @param param_name   The name of the parameter such that params->get_param(param_name)
   *                     returns the string that will map to the child class
   * @param defval    The default value to use in case the parameter has not been specified
   * @param params    The parameters potentially used in the constructor
   * @param args      The required arguments for the constructor
   * @return  A constructed child class corresponding to a given string name
   */
  static T*
  get_optional_param(const std::string& param_name,
                     const std::string& defval,
                     sim_parameters* params,
                     const Args&... args) {
    return _get_value(params->get_optional_param(param_name, defval),
                      params, args...);
  }

};

template<class Child, typename Parent, typename... Args>
class SpktBuilderImpl<Child, Factory<Parent, Args...> > :
  public SpktBuilder<Parent, Args...>
{
 public:
  SpktBuilderImpl(const char *name){
    Factory<Parent, Args...>::register_name(name, this);
  }

  Parent*
  build(sprockit::sim_parameters* params, const Args&... args) {
    return call_constructor<Child,Args...>()(params, args...);
  }

};


template <class Factory>
class CleanupFactory {
 public:
  ~CleanupFactory(){
    Factory::clean_up();
  }
};

}

#define FirstArgStr(X, ...) #X
#define FirstArgFactoryName(X, ...) X##_factory

#define DeclareFactory(...) \
  typedef ::sprockit::Factory<__VA_ARGS__> FirstArgFactoryName(__VA_ARGS__);

#define ImplementFactory(type_name) \
  template<> const char* type_name##_factory::name_ = #type_name; \
  template<> std::map<std::string, type_name##_factory::builder_t*>* type_name##_factory::builder_map_ = nullptr; \
  template<> std::map<std::string, std::list<std::string>>* type_name##_factory::alias_map_ = nullptr; \
  namespace { static sprockit::CleanupFactory<type_name##_factory> cleaner; }



#define SpktTemplateRegister(cls_str, parent_cls, child_cls, unique_name, ...) \
    static ::sprockit::SpktBuilderImpl<child_cls, parent_cls##_factory> unique_name##_cd(cls_str)

#define SpktRegister(cls_str, parent_cls, child_cls, ...) \
  static ::sprockit::SpktBuilderImpl<child_cls,parent_cls##_factory> child_cls##_cd(cls_str)


#define DeclareFactory1InitParam(type_name, param1_name) \
  DeclareFactory(type_name, param1_name);

#define DeclareFactory2InitParams(type_name, param1_name, param2_name) \
  DeclareFactory(type_name, param1_name, param2_name);

#define DeclareFactory3InitParams(type_name, param1_name, param2_name, param3_name) \
  DeclareFactory(type_name, param1_name, param2_name, param3_name);

#endif

