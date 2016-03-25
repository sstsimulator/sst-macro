#ifndef sprockit_keyword_registration_H
#define sprockit_keyword_registration_H

#include <sprockit/spkt_config.h>
#include <sprockit/unordered.h>

#include <string>
#include <cstdio>
#include <list>

namespace sprockit {

class KeywordRegistration
{

 private:
  static spkt_unordered_set<std::string>* valid_keywords_;

  static spkt_unordered_set<std::string>* valid_namespaces_;

#if !SPKT_DISABLE_REGEX
  static std::list<std::string>* regexps_;
#endif

  static spkt_unordered_set<std::string>* removed_;

  static bool inited_;

  static void init();

 public:
  static void register_regexp(const std::string& regexp);

  static void register_namespace(const std::string& ns);

  static void register_keyword(const std::string& name);

  static bool is_valid_keyword(const std::string& name);

  static bool is_valid_namespace(const std::string& ns);

  static void validate_namespace(const std::string& ns);

  static void validate_keyword(const std::string& name, const std::string& val);

  static void delete_statics();

  static bool do_validation_;

};

class StaticNamespaceRegister
{
 public:
  StaticNamespaceRegister(const char* ns);

  StaticNamespaceRegister(int num_ns, const char* namespaces[]);
};

class StaticKeywordRegister
{
 public:
  StaticKeywordRegister(int num_keywords, const char* keywords[]);
};


#if !SPKT_DISABLE_REGEX
class StaticKeywordRegisterRegexp
{
 public:
  StaticKeywordRegisterRegexp(const char* regexp);
};
#endif

}

#define RegisterKeywords(...) \
  static const char* _keywords_[] = { __VA_ARGS__ }; \
  static ::sprockit::StaticKeywordRegister _keyword_register_(sizeof(_keywords_) / sizeof(const char*), _keywords_)

#define RegisterNamespaces(...) \
  static const char* _namespaces_[] = { __VA_ARGS__ }; \
  static ::sprockit::StaticNamespaceRegister _namespace_register_(sizeof(_namespaces_) / sizeof(const char*), _namespaces_)

#endif

