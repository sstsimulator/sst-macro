#ifndef bin_clang_globalvarnamespace_H
#define bin_clang_globalvarnamespace_H

#include <string>
#include <set>
#include <map>
#include <ostream>
#include <sstream>

struct GlobalVarNamespace
{
  GlobalVarNamespace() : isPrefixSet(false) {}

  std::string ns;
  std::set<std::string> replVars;
  std::map<std::string, GlobalVarNamespace> subspaces;
  char uniqueFilePrefix[256];
  bool isPrefixSet;

  bool empty() const {
    return replVars.empty() && subspaces.empty();
  }

  void setFilePrefix(const char* name){
    ::strcpy(uniqueFilePrefix, name);
    int len = ::strlen(uniqueFilePrefix);
    for (int i=0; i < len; ++i){
      switch (uniqueFilePrefix[i]){
        case '-':
        case '/':
        case '.':
          uniqueFilePrefix[i] = '_';
          break;
      }
    }
    isPrefixSet = true;
  }

  void appendNamespace(const std::string& nestedNS, const std::string& newNS){
    if (ns.size() == 0){
      ns = nestedNS + "::" + newNS + "::";
    }
  }

  const std::string& nsPrefix() const {
    return ns;
  }

  const char* filePrefix() const {
    return uniqueFilePrefix;
  }

  bool genSSTCode(std::ostream& os, const std::string& indent){
    bool nonEmpty = !replVars.empty();
    for (const std::string& var : replVars){
      os << indent << "int __offset_" << var << " = 0;\n";
      os << indent << "extern int __sizeof_" << var << ";\n";
      os << indent << "extern void* __ptr_" << var << ";\n";
      os << indent << "sstmac::GlobalVariable __gv_" << var
              << "(__offset_" << var
              << ",__sizeof_" << var
              << ",__ptr_" << var
              << ");\n";
    }
    for (auto& pair : subspaces){
      std::stringstream sstr;
      bool subNotEmpty = false;
      if (!pair.second.empty()){
        sstr << indent << "namespace " << pair.first << " {\n";
        subNotEmpty |= pair.second.genSSTCode(sstr, indent + " ");
        sstr << indent << "}\n";
      }
      if (subNotEmpty) os << sstr.str();
      nonEmpty |= subNotEmpty;
    }
    return nonEmpty;
  }

};

#endif
