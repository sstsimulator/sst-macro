#include <sstmac/software/process/api.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/statics.h>

namespace sstmac {
namespace sw {

int APIBase::id_counter = 0;
spkt_unordered_map<std::string, int>* APIBase::name_to_id = 0;
static sprockit::need_delete_statics<APIBase> del_statics;

APIBase::APIBase(const char *name)
{
  if (!name_to_id) {
    name_to_id = new spkt_unordered_map<std::string,int>;
  }
  (*name_to_id)[name] = id_counter;
}

void
APIBase::delete_statics()
{
  delete name_to_id;
}

int
APIBase::api_id(const std::string &name)
{
  spkt_unordered_map<std::string,int>::const_iterator it
    = name_to_id->find(name);
  if (it == name_to_id->end()) {
    spkt_throw_printf(sprockit::value_error, "invalid api name %s", name.c_str());
  }
  return it->second;
}

}
}

