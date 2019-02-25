// Copyright 2009-2018 NTESS. Under the terms
// of Contract DE-NA0003525 with NTESS, the U.S.
// Government retains certain rights in this software.
// 
// Copyright (c) 2009-2018, NTESS
// All rights reserved.
// 
// This file is part of the SST software package. For license
// information, see the LICENSE file in the top level directory of the
// distribution.

#ifndef spkt_spkt_factory_h_
#define spkt_spkt_factory_h_

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <cstdlib>
#include <memory>
#include <sprockit/sim_parameters_fwd.h>

namespace sprockit {

template <class Base, class... Args>
struct Factory
{
  typedef Base* (*createFxn)(Args...);

  virtual Base* create(Args... ctorArgs) = 0;
};

template <class Base, class T, class Enable=void>
struct Allocator {
  template <class... Args>
  Base* operator()(Args&&... args){
    return new T(std::forward<Args>(args)...);
  }
};

template <class T> struct wrap { using type=void; };

template <class Base, class T>
struct Allocator<Base,T,
  typename wrap<decltype(std::declval<T>().finalizeInit(std::declval<SST::Params&>()))>::type>
{
  template <class... Args>
  Base* operator()(SST::Params& params, Args&&... args){
    T* t = new T(params, std::forward<Args>(args)...);
    t->finalizeInit(params);
    return t;
  }
};


template <class Base, class T, class... Args>
struct CachedAllocator
{
  Base* operator()(Args... ctorArgs) override {
    if (!cached_){
      cached_ = new T(std::forward<Args>(ctorArgs)...);
    }
    return cached_;
  }

  static Base* cached_;
};
template <class Base, class T, class... Args>
  Base* CachedAllocator<Base,T,Args...>::cached_ = nullptr;

template <class Base, class T, class... Args>
struct DerivedFactory : public Factory<Base,Args...>
{
  Base* create(Args... ctorArgs) override {
    return Allocator<Base,T>()(std::forward<Args>(ctorArgs)...);
  }
};

template <class Base, class T>
struct Instantiate {
  static bool isLoaded() {
    return loaded;
  }

  static const bool loaded;
};

template <class Base, class... CtorArgs>
class FactoryLibrary
{
 public:
  using BaseFactory = Factory<Base,CtorArgs...>;

  BaseFactory* getFactory(const std::string &name) {
    auto iter = factories_.find(name);
    if (iter == factories_.end()){
      return nullptr;
    } else {
      return iter->second;
    }
  }

  const std::map<std::string, BaseFactory*>& getMap() const {
    return factories_;
  }

  void addFactory(const std::string& name, BaseFactory* fact){
    factories_[name] = fact;
  }

 private:
  std::map<std::string, BaseFactory*> factories_;
};

template <class Base, class... CtorArgs>
class FactoryLibraryDatabase {
 public:
  using Library=FactoryLibrary<Base,CtorArgs...>;
  using BaseFactory=typename Library::BaseFactory;

  static Library* getLibrary(const std::string& name){
    if (!libraries){
      libraries = std::unique_ptr<std::map<std::string,Library*>>(new std::map<std::string,Library*>);
    }
    auto iter = libraries->find(name);
    if (iter == libraries->end()){
      auto* info = new Library;
      (*libraries)[name] = info;
      return info;
    } else {
      return iter->second;
    }
  }

 private:
  // Database - needs to be a pointer for static init order
  static std::unique_ptr<std::map<std::string,Library*>> libraries;
};

template <class Base, class... CtorArgs>
 std::unique_ptr<std::map<std::string,FactoryLibrary<Base,CtorArgs...>*>>
  FactoryLibraryDatabase<Base,CtorArgs...>::libraries;

template <class T, class U>
struct is_tuple_constructible : public std::false_type {};

template <class T, class... Args>
struct is_tuple_constructible<T, std::tuple<Args...>> :
  public std::is_constructible<T, Args...>
{
};

template <class Base, class CtorTuple>
struct ElementsFactory {};

template <class Base, class... Args>
struct ElementsFactory<Base, std::tuple<Args...>>
{
  static FactoryLibrary<Base,Args...>* getLibrary(const std::string& name){
    return FactoryLibraryDatabase<Base,Args...>::getLibrary(name);
  }

  template <class T> static Factory<Base,Args...>* makeFactory(){
    return new DerivedFactory<Base,T,Args...>();
  }

};

template <class Base, class Ctor, class... Ctors>
struct CtorList : public CtorList<Base,Ctors...>
{
  template <int NumValidCtors, class T, class U=T>
  static typename std::enable_if<is_tuple_constructible<U,Ctor>::value, bool>::type
  add(){
    auto* fact = ElementsFactory<Base,Ctor>::template makeFactory<U>();
    ElementsFactory<Base,Ctor>::getLibrary(T::SPKT_getLibrary())->addFactory(T::SPKT_getName(), fact);
    return CtorList<Base,Ctors...>::template add<NumValidCtors+1,T>();
  }

  template <int NumValidCtors, class T, class U=T>
  static typename std::enable_if<!is_tuple_constructible<U,Ctor>::value, bool>::type
  add(){
    return CtorList<Base,Ctors...>::template add<NumValidCtors,T>();
  }

  //Ctor is a tuple
  template <class... InArgs>
  typename std::enable_if<std::is_convertible<std::tuple<InArgs&&...>, Ctor>::value, Base*>::type
  operator()(const std::string& elemlib, const std::string& name, InArgs&&... args){
    auto* lib = ElementsFactory<Base,Ctor>::getLibrary(elemlib);
    if (lib){
      auto* fact = lib->getFactory(name);
      if (fact){
        return fact->create(std::forward<InArgs>(args)...);
      }
      std::cerr << "For library '" << elemlib << "' with base " << Base::SPKT_baseName()
               << ", derived '" << name << "' does not provide matching constructor"
               << std::endl;
    }
    std::cerr << "No matching constructors found in library " << elemlib
              << " for base " << Base::SPKT_baseName() << std::endl;
    ::abort();
  }

  //Ctor is a tuple
  template <class... InArgs>
  typename std::enable_if<!std::is_convertible<std::tuple<InArgs&&...>, Ctor>::value, Base*>::type
  operator()(const std::string& elemlib, const std::string& name, InArgs&&... args){
    //I don't match - pass it up
    return CtorList<Base,Ctors...>::operator()(elemlib, name, std::forward<InArgs>(args)...);
  }

};

template <int NumValid>
struct NoValidConstructorsForDerivedType {
  static constexpr bool atLeastOneValidCtor = true;
};

template <> class NoValidConstructorsForDerivedType<0> {};

template <class Base> struct CtorList<Base,void>
{
  template <int numValidCtors, class T>
  static bool add(){
    return NoValidConstructorsForDerivedType<numValidCtors>::atLeastOneValidCtor;
  }
};

template <class Base, class T>
 const bool Instantiate<Base,T>::loaded = Base::Ctor::template add<0,T>();

struct FactoryDatabase {
  template <class T, class... Args>
  static FactoryLibrary<T,Args...>* getLibrary(const std::string& name){
    return FactoryLibraryDatabase<T,Args...>::getLibrary(name);
  }
};

} //namespace sprockit

#define SPKT_FORWARD_AS_ONE(...) __VA_ARGS__

#define SPKT_CTOR(...) std::tuple<__VA_ARGS__>
#define SPKT_DEFAULT_CTOR() std::tuple<>

#define SPKT_REGISTER_COMMON(Base) \
  using __LocalBaseName = Base; \
  template <class... __Args> \
  using FactoryLibrary = sprockit::FactoryLibrary<Base,__Args...>; \
  static const char* SPKT_baseName(){ return #Base; }

#define SPKT_REGISTER_BASE(Base) \
  SPKT_REGISTER_COMMON(Base)

#define SPKT_REGISTER_CTORS_COMMON(...) \
  using Ctor = sprockit::CtorList<__LocalBaseName,__VA_ARGS__,void>; \
  template <class... InArgs> static __LocalBaseName* create( \
    const std::string& elemlib, const std::string& elem, InArgs&&... args){ \
    return Ctor()(elemlib, elem, std::forward<InArgs>(args)...); \
  }

#define SPKT_REGISTER_CTORS(...) \
  SPKT_REGISTER_CTORS_COMMON(__VA_ARGS__) \
  template <class __TT, class... __CtorArgs> \
  using DerivedFactory = sprockit::DerivedFactory<__LocalBaseName,__TT,__CtorArgs...>; \
  template <class... __InArgs> static FactoryLibrary<__InArgs...>* getFactoryLibrary(const std::string& name){ \
    return sprockit::FactoryDatabase::getLibrary<__LocalBaseName,__InArgs...>(name); \
  }

//I can make some extra using typedefs because I have only a single ctor
#define SPKT_REGISTER_CTOR(...) \
  SPKT_REGISTER_CTORS_COMMON(SPKT_CTOR(__VA_ARGS__)) \
  template <class __TT> \
  using DerivedFactory = sprockit::DerivedFactory<__LocalBaseName,__TT,__VA_ARGS__>; \
  static FactoryLibrary<__VA_ARGS__>* getFactoryLibrary(const std::string& name){ \
    return sprockit::FactoryDatabase::getLibrary<__LocalBaseName,__VA_ARGS__>(name); \
  }

//I can make some extra using typedefs because I have only a single ctor
#define SPKT_REGISTER_DEFAULT_CTOR() \
  SPKT_REGISTER_CTORS_COMMON(SPKT_DEFAULT_CTOR()) \
  template <class __TT> \
  using DerivedFactory = sprockit::DerivedFactory<__LocalBaseName,__TT>; \
  static FactoryLibrary<>* getFactoryLibrary(const std::string& name){ \
    return sprockit::FactoryDatabase::getLibrary<__LocalBaseName>(name); \
  }

#define SPKT_REGISTER_DERIVED(base,cls,lib,name,version,desc) \
  static const char* SPKT_getLibrary(){ \
    return lib; \
  } \
  static const char* SPKT_getName(){ \
    return name; \
  } \
  bool SPKT_isLoaded() { \
      return sprockit::Instantiate<base,cls>::isLoaded(); \
  } 

#endif

#endif 

