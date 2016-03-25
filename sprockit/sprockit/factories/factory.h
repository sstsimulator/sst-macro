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
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/spkt_config.h>
#include <map>

namespace sprockit {

class factory_type  {

 public:
  virtual void
  init_factory_params(sim_parameters* params);

  virtual void
  finalize_init() {
  }

};

class SpktDesc_base
{
 public:
  virtual factory_type*
  build() = 0;

  virtual void
  clear();

};

class SpktFactory_base
{
 public:
  static void
  add_to_map(const std::string& namestr, SpktDesc_base* desc,
             std::map<std::string, SpktDesc_base*> *m);

  static std::string
  value(const std::string& key, sim_parameters* params);

  static std::string
  value(const std::string& key, const std::string& defval, sim_parameters* params);

  static bool
  exists(const std::string& key, sim_parameters* params);

};

template<class T>
class SpktFactoryTemplateBase : public SpktFactory_base
{
 protected:
  typedef std::map<std::string, SpktDesc_base*> descr_map;
  static descr_map* object_map_;
  static const char* name_;

 public:
  static void
  register_name(const std::string& name, SpktDesc_base* descr) {
    if (!object_map_) {
      object_map_ = new descr_map;
    }
    add_to_map(name, descr, object_map_);
  }

  static void
  delete_statics(){
    delete object_map_;
  }

  static void
  validate_map(const std::string& valname) {
    if (!object_map_) {
      spkt_throw_printf(illformed_error,
                       "could not find name %s for factory %s. no classes are registered",
                       valname.c_str(),
                       name_);
    }
  }
};

template <class T>
void
factory_init(T* t,
             sim_parameters* params)
{
  t->init_factory_params(params);
  t->finalize_init();
}

/**
  When initializing, you must initialized object members first.
  Then go ahead and initialized factory values from the parameters.
*/
template <class T, class A>
void
factory_init(T* t,
             sim_parameters* params, const A& a)
{
  t->init_param1(a);
  t->init_factory_params(params);
  t->finalize_init();
}

template <class T, class A, class B>
void
factory_init(T* t,
             sim_parameters* params, const A& a, const B& b)
{
  t->init_param1(a);
  t->init_param2(b);
  t->init_factory_params(params);
  t->finalize_init();
}

template <class T, class A, class B, class C>
void
factory_init(T* t,
             sim_parameters* params, const A& a, const B& b, const C& c)
{
  t->init_param1(a);
  t->init_param2(b);
  t->init_param3(c);
  t->init_factory_params(params);
  t->finalize_init();
}

template<class T>
class SpktFactory : public SpktFactoryTemplateBase<T>
{

 public:
  typedef SpktFactoryTemplateBase<T> parent;
  typedef T element_type;
  using SpktFactoryTemplateBase<T>::object_map_;
  using SpktFactoryTemplateBase<T>::name_;

 protected:
  static T*
  _get_value(const std::string& valname) {
    parent::validate_map(valname);

    std::map<std::string, SpktDesc_base*>::const_iterator it =
      object_map_->find(valname), end = object_map_->end();

    if (it == end) {
      std::cerr << "Valid factories are:" << std::endl;
      for (it = object_map_->begin(); it != end; ++it) {
        std::cerr << it->first << std::endl;
      }
      spkt_throw_printf(value_error, "could not find name %s for factory %s",
                       valname.c_str(), name_);
    }

    SpktDesc_base* descr = it->second;
    if (!descr) {
      spkt_throw_printf(value_error,
                       "initialized name %s with null descr for factory %s",
                       valname.c_str(), name_);
    }

    factory_type* p = descr->build();
#if SPKT_ENABLE_PARAM_RECORDING
    p->recorder()->record_factory_init(parent::name_, valname.c_str());
#endif
    T* cast = dynamic_cast<T*>(p);
    return cast;
  }

  static T*
  _get_param(const std::string& param_name,
             sim_parameters* params) {
    T* rv = _get_value(parent::value(param_name, params));
    return rv;

  }

  static T*
  _get_optional_param(const std::string& param_name,
                      const std::string& defval, sim_parameters* params) {
    T* rv = _get_value(parent::value(param_name, defval, params));
    return rv;
  }

 public:
  static T*
  get_value(const std::string& valname, sim_parameters* params) {
    T* p = _get_value(valname);
    factory_init(p, params);
    return p;
  }

  static T*
  get_extra_param(const std::string& param_name,
                  sim_parameters* params) {
    if (parent::exists(param_name, params)) {
      return get_param(param_name,params);
    }
    else {
      return 0;
    }
  }

  static T*
  get_param(const std::string& param_name,
            sim_parameters* params) {
    T* p = _get_param(param_name, params);
    factory_init(p, params);
    return p;
  }

  static T*
  get_optional_param(const std::string& param_name,
                     const std::string& defval,
                     sim_parameters* params) {
    T* p = _get_optional_param(param_name, defval, params);
    factory_init(p, params);
    return p;
  }

};


template<class T, class U>
class SpktFactory1InitParam : public SpktFactory<T>
{
 public:
  typedef SpktFactory<T> parent;
  using parent::get_value;
  using parent::get_param;

 public:
  static T*
  get_value(const std::string& valname, sim_parameters* params,
            const U& param1) {
    T* p = parent::_get_value(valname);
    factory_init(p, params, param1);
    return p;
  }

  static T*
  get_param(const std::string& param_name,
            sim_parameters* params, const U& param1) {
    T* p = parent::_get_param(param_name, params);
    factory_init(p, params, param1);
    return p;
  }

  static T*
  get_extra_param(const std::string& param_name,
                  sim_parameters* params,
                  const U& param1) {
    if (parent::exists(param_name, params)) {
      return get_param(param_name,params,param1);
    }
    else {
      return 0;
    }
  }

  static T*
  get_optional_param(const std::string& param_name,
                     const std::string& defval, sim_parameters* params,
                     const U& param1) {
    T* p = parent::_get_optional_param(param_name, defval, params);
    factory_init(p, params, param1);
    return p;
  }
};

template<class T, class U, class V>
class SpktFactory2InitParams : public SpktFactory<T>
{
 public:
  typedef SpktFactory<T> parent;
  using parent::get_value;
  using parent::get_param;

 public:
  static T*
  get_value(const std::string& valname, sim_parameters* params,
            const U& param1, const V& param2) {
    T* p = parent::_get_value(valname);
    factory_init(p, params, param1, param2);
    return p;
  }

  static T*
  get_extra_param(const std::string& param_name,
                  sim_parameters* params,
                  const U& param1, const V& param2) {
    if (parent::exists(param_name, params)) {
      return get_param(param_name,params,param1,param2);
    }
    else {
      return 0;
    }
  }

  static T*
  get_param(const std::string& param_name,
            sim_parameters* params, const U& param1, const V& param2) {
    T* p = parent::_get_param(param_name, params);
    factory_init(p, params, param1, param2);
    return p;
  }

  static T*
  get_optional_param(const std::string& param_name,
                     const std::string& defval, sim_parameters* params,
                     const U& param1, const V& param2) {
    T* p = parent::_get_optional_param(param_name, defval, params);
    factory_init(p, params, param1, param2);
    return p;
  }
};

template<class T, class U, class V, class W>
class SpktFactory3InitParams : public SpktFactory<T>
{
 public:
  typedef SpktFactory<T> parent;
  using parent::get_value;
  using parent::get_param;

 public:
  static T*
  get_value(const std::string& valname, sim_parameters* params,
            const U& param1, const V& param2, const W& param3) {
    T* p = parent::_get_value(valname);
    factory_init(p, params, param1, param2, param3);
    return p;
  }

  static T*
  get_param(const std::string& param_name,
            sim_parameters* params, const U& param1, const V& param2,
            const W& param3) {
    T* p = parent::_get_param(param_name, params);
    factory_init(p, params, param1, param2, param3);
    return p;
  }

  static T*
  get_optional_param(const std::string& param_name,
                     const std::string& defval, sim_parameters* params,
                     const U& param1, const V& param2, const W& param3) {
    T* p = parent::_get_optional_param(param_name, defval, params);
    factory_init(p, params, param1, param2, param3);
    return p;
  }

  static T*
  get_extra_param(const std::string& param_name,
                  sim_parameters* params,
                  const U& param1, const V& param2, const W& param3) {
    if (parent::exists(param_name, params)) {
      return get_param(param_name,params,param1,param2,param3);
    }
    else {
      return 0;
    }
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

template<class Parent, class Child>
class SpktFactory_desc : public SpktDesc_base
{

 public:
  SpktFactory_desc(const char* name) {
    std::string n(name);
    /** This should only ever be declared within
     the static initialization routine. There is therefore
     nothing unsafe about passing "this" out.  This object
     will be persisent throughout the entire simulation */
    SpktFactory<Parent>::register_name(n, this);
  }

  factory_type*
  build() {
    return new Child;
  }

};

}

#define DeclareFactory(type_name) \
    typedef ::sprockit::SpktFactory<type_name> type_name##_factory;

#define DeclareFactory1Param(type_name, param1_name) \
    typedef ::sprockit::SpktFactory1Param<type_name, param1_name> type_name##_factory;

#define DeclareFactory1InitParam(type_name, param1_name) \
    typedef ::sprockit::SpktFactory1InitParam<type_name, param1_name> type_name##_factory;

#define DeclareFactory2InitParams(type_name, param1_name, param2_name) \
    typedef ::sprockit::SpktFactory2InitParams<type_name, param1_name, param2_name> type_name##_factory;

#define DeclareFactory3InitParams(type_name, param1_name, param2_name, param3_name) \
    typedef ::sprockit::SpktFactory3InitParams<type_name, param1_name, param2_name, param3_name> type_name##_factory;

#define ImplementFactory(type_name) \
    namespace sprockit { \
    template<> const char* ::sprockit::SpktFactoryTemplateBase<type_name>::name_ = #type_name; \
    template<> std::map<std::string, SpktDesc_base*>* SpktFactoryTemplateBase<type_name>::object_map_ = 0; \
    static need_delete_statics<SpktFactory< type_name > > factory_del_statics; \
    }

#define SpktTemplateRegister(cls_str, parent_cls, child_cls, unique_name) \
    static ::sprockit::SpktFactory_desc<parent_cls,child_cls> unique_name##_cd(cls_str)

#define SpktRegister(cls_str, parent_cls, child_cls, ...) \
  SpktTemplateRegister(cls_str, parent_cls, child_cls, child_cls)


#endif

