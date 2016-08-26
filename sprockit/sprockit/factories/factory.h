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

class factory_type  {

 public:
  virtual void
  init_factory_params(sim_parameters* params){
  }

  virtual void
  finalize_init() {
  }

};

template <class T, typename... Args>
class SpktBuilder
{
 public:
  virtual T*
  build(const Args&... args) = 0;

};

template<class Child, class Factory>
class SpktBuilderImpl
{
};

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

  static void
  register_name(const std::string& name, builder_t* descr) {
    if (!builder_map_) {
      builder_map_ = new builder_map;
    }
    if (!alias_map_){
      alias_map_ = new alias_map;
    }
    add_to_map(name, descr, builder_map_, alias_map_);
  }

  static void
  add_to_map(const std::string& namestr, builder_t* desc,
            std::map<std::string, builder_t*>* descr_map,
            std::map<std::string, std::list<std::string> >* alias_map)
  {
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


 protected:
  static T*
  _get_value(const std::string& valname,
             sprockit::sim_parameters* params,
             const Args&... args) {
    if (!builder_map_) {
      spkt_throw_printf(illformed_error,
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
      spkt_throw_printf(value_error, "could not find name %s for factory %s",
                       valname.c_str(), name_);
    }

    builder_t* descr = it->second;
    if (!descr) {
      spkt_throw_printf(value_error,
                       "initialized name %s with null descr for factory %s",
                       valname.c_str(), name_);
    }

    T* p = descr->build(args...);
    p->init_factory_params(params);
    p->finalize_init();
    return p;
  }

 public:
  static T*
  get_value(const std::string& valname,
            sim_parameters* params,
            const Args&... args) {
    return _get_value(valname, params, args...);
  }

  static T*
  get_extra_param(const std::string& param_name,
                  sim_parameters* params,
                  const Args&... args) {
    if (params->has_param(param_name)) {
      return get_param(param_name,params);
    }
    else {
      return 0;
    }
  }

  static T*
  get_param(const std::string& param_name,
            sim_parameters* params,
            const Args&... args) {
    return _get_value(params->get_param(param_name),
                      params, args...);
  }

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
  build(const Args&... args) {
    return new Child(args...);
  }

};

template <class T>
class factory
{
 public:
  virtual T* build(sim_parameters* params) = 0;

  virtual ~factory(){}
};

template <class T, class Factory>
class template_factory : public factory<T>
{
 public:
  template_factory(const std::string& param_name)
    : param_name_(param_name)
  {
  }

  T* build(sim_parameters* params){
    typedef typename Factory::element_type F;
    F* f = Factory::get_value(param_name_, params);
    return f;
  }

 private:
  std::string param_name_;
};

template <class T>
class factory2
{
 public:
  virtual ~factory2(){}

  virtual T* build(sim_parameters* params, sprockit::factory_type* ft) = 0;
};

template <class T, class Factory>
class template_factory2 : public factory2<T>
{
 public:
  template_factory2(const std::string& param_name)
    : param_name_(param_name)
  {
  }

  T* build(sim_parameters* params, sprockit::factory_type* ft){
    typedef typename Factory::element_type F;
    F* f = Factory::get_value(param_name_, params, ft);
    return f;
  }

 private:
  std::string param_name_;
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
  template<> std::map<std::string, type_name##_factory::builder_t*>* type_name##_factory::builder_map_ = 0; \
  template<> std::map<std::string, std::list<std::string>>* type_name##_factory::alias_map_ = 0; \
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

