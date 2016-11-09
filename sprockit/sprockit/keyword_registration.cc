#include <sprockit/spkt_config.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/regexp.h>
#include <sprockit/errors.h>
#include <sprockit/output.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/unordered.h>

#include <cstdio>

namespace sprockit {

spkt_unordered_set<std::string>* KeywordRegistration::valid_keywords_ = 0;
spkt_unordered_set<std::string>* KeywordRegistration::valid_namespaces_ = 0;
spkt_unordered_set<std::string>* KeywordRegistration::removed_ = 0;
#if !SPKT_DISABLE_REGEX
std::list<std::string>* KeywordRegistration::regexps_ = 0;
#endif
bool KeywordRegistration::inited_ = false;
#if !SPKT_DISABLE_REGEX
bool KeywordRegistration::do_validation_ = true;
#else
bool KeywordRegistration::do_validation_ = false;
#endif
static need_delete_statics<KeywordRegistration> del_statics;

static const char* removed_keywords[] = {
  "launch_name"
};

void
KeywordRegistration::delete_statics()
{
  if (valid_keywords_) delete valid_keywords_;
  if (removed_) delete removed_;
  if (regexps_) delete regexps_;
  if (valid_namespaces_) delete valid_namespaces_;
}

bool
KeywordRegistration::is_valid_namespace(const std::string& ns)
{
  init();
  auto it = valid_namespaces_->find(ns);
  if (it != valid_namespaces_->end()) {
    return true;
  }

#if !SPKT_DISABLE_REGEXP
  //otherwise, check to see if integer
  if (has_regexp_match("\\d+", ns)){
    return true;
  }
  return false;
#else
  //hmm, no choice, treat it as valid
  return true;
#endif
}

bool
KeywordRegistration::is_valid_keyword(const std::string &name)
{
  init();
  auto it = valid_keywords_->find(name);
  if (it != valid_keywords_->end()) {
    return true;
  }

#if !SPKT_DISABLE_REGEXP
  for (auto& re : *regexps_){
    if (has_regexp_match(re, name)){
      return true;
    }
  }
  return false;
#else
  //if here, well, it might be a regexp but we just don't know!
  return true;
#endif

}

void
KeywordRegistration::register_namespace(const std::string &name)
{
  init();
  valid_namespaces_->insert(name);
}

void
KeywordRegistration::register_keyword(const std::string &name)
{
  init();
  valid_keywords_->insert(name);
}

void
KeywordRegistration::register_regexp(const std::string &regexp)
{
  init();
  regexps_->push_back(regexp);
}

void
KeywordRegistration::init()
{
  if (inited_) {
    return;
  }

  valid_keywords_ = new spkt_unordered_set<std::string>;
  valid_namespaces_ = new spkt_unordered_set<std::string>;
  regexps_ = new std::list<std::string>;

  inited_ = true;

  register_regexp("launch_app\\d+");
  register_regexp("launch_app\\d+_size");
  register_regexp("launch_app\\d+_start");
  register_regexp("launch_app\\d+_ntask_per_node");
  register_regexp("launch_app\\d+_argv");
  register_regexp("launch_app\\d+_cmd");
  register_regexp("launch_app\\d+_type");

  removed_ = new spkt_unordered_set<std::string>;
  int num_removed = sizeof(removed_keywords) / sizeof(const char*);
  for (int i=0; i < num_removed; ++i) {
    removed_->insert(removed_keywords[i]);
  }

}

void
KeywordRegistration::validate_namespace(const std::string &ns)
{
  if (do_validation_){
    bool valid = is_valid_namespace(ns);
    if (!valid) {
      spkt_abort_printf("namespace %s is not valid - if this is not a type-o ensure that "
                        "namespace was properly registered with RegisterNamespaces(...) macro",
                        ns.c_str());
    }
  }
}

void
KeywordRegistration::validate_keyword(const std::string &name,
                                      const std::string &val)
{
  if(do_validation_) {
    bool valid = is_valid_keyword(name);
    if (!valid) {
      bool removed = removed_->find(name) != removed_->end();
      if (removed) {
        cerr0 << "WARNING: deprecated keyword " << name <<
                  " is no longer used and is being ignored.\n"
                  << "You should remove it from parameter files as it may become an error in future versions.\n";
      }
      else {
        spkt_abort_printf("unknown keyword name %s with value %s - if this is not a type-o ensure that "
                          "keyword was properly registered with RegisterKeywords(...) macro",
                         name.c_str(), val.c_str());
      }
    }
  }
}

StaticNamespaceRegister::StaticNamespaceRegister(const char *ns)
{
  KeywordRegistration::register_namespace(ns);
}

StaticNamespaceRegister::StaticNamespaceRegister(int num_ns, const char *namespaces[])
{
  for (int i=0; i < num_ns; ++i){
    const char* ns = namespaces[i];
    KeywordRegistration::register_namespace(ns);
  }
}

StaticKeywordRegisterRegexp::StaticKeywordRegisterRegexp(const char *regexp)
{
  KeywordRegistration::register_regexp(regexp);
}

StaticKeywordRegister::StaticKeywordRegister(int num_keywords,
    const char *keywords[])
{
  for (int i=0; i < num_keywords; ++i) {
    const char* keyword = keywords[i];
    KeywordRegistration::register_keyword(keyword);
  }
}

}

