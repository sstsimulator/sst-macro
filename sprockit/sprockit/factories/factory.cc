#include <sprockit/factories/factory.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/sim_parameters.h>

namespace sprockit {

void
factory_type::init_factory_params(sim_parameters *params)
{
}

void
SpktDesc_base::clear()
{
}

std::string
SpktFactory_base::value(const std::string &key, const std::string &defval, sim_parameters *params)
{
  return params->get_optional_param(key, defval);
}

std::string
SpktFactory_base::value(const std::string &key, sim_parameters *params)
{
  return params->get_param(key);
}

bool
SpktFactory_base::exists(const std::string &key, sim_parameters *params)
{
  return params->has_param(key);
}

void
SpktFactory_base::add_to_map(const std::string& namestr, SpktDesc_base* desc,
                            std::map<std::string, SpktDesc_base*>* m)
{
  std::string space = "|";
  std::deque<std::string> tok;
  pst::BasicStringTokenizer::tokenize(namestr, tok, space);

  std::deque<std::string>::iterator it, end = tok.end();

  for (it = tok.begin(); it != end; it++) {
    std::string temp = *it;

    temp = trim_str(temp);

    (*m)[temp] = desc;

  }
}

}

