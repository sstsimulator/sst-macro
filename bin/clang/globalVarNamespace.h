#ifndef bin_clang_globalvarnamespace_H
#define bin_clang_globalvarnamespace_H

#include <string>
#include <set>
#include <map>
#include <ostream>

struct GlobalVarNamespace
{
  GlobalVarNamespace() : isPrefixSet(false) {}

  std::string ns;
  std::set<std::string> vars;
  std::map<std::string, GlobalVarNamespace> subspaces;
  char uniqueFilePrefix[256];
  bool isPrefixSet;

  bool empty() const {
    return vars.empty() && subspaces.empty();
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

  void genSSTCode(std::ostream& os, const std::string& indent){
    for (const std::string& var : vars){
      os << indent << "int __offset_" << var << " = 0;\n";
      os << indent << "extern const int __sizeof_" << var << ";\n";
      os << indent << "extern void* __ptr_" << var << ";\n";
      os << indent << "sstmac::GlobalVariable __gv_" << var
              << "(__offset_" << var
              << ",__sizeof_" << var
              << ",__ptr_" << var
              << ");\n";
    }
    for (auto& pair : subspaces){
      os << indent << "namespace " << pair.first << " {\n";
      pair.second.genSSTCode(os, indent + " ");
      os << indent << "}\n";
    }
  }

};

#endif
