
#include <sst/core/component.h>
#include <sst/core/linkMap.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/sst_core/connectable_wrapper.h>

#include <sprockit/output.h>

namespace sstmac {

SST::TimeConverter* SSTIntegratedComponent::time_converter_ = 0;

SSTIntegratedComponent::SSTIntegratedComponent(
    SST::ComponentId_t id,
    SST::Params& params
) : SST::Component(id),
  self_link_(0)
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  params_ = make_sim_params_from_params(params);

  link_map_ = SST::Simulation::getSimulation()->getComponentLinkMap(id);
  if (!time_converter_){
    time_converter_ = getTimeConverter(timestamp::tick_interval_string());
  }
}

void
SSTIntegratedComponent::configure_self_link()
{
  self_link_ = configureSelfLink("self", time_converter_, 
    new SST::Event::Handler<SSTIntegratedComponent>(this, &SSTIntegratedComponent::handle_self_link));
}

void
SSTIntegratedComponent::init(unsigned int phase)
{
  if (phase == 0){
    init_factory_params(params_);
  }
}

void
SSTIntegratedComponent::handle_self_link(SST::Event* ev)
{
  SSTSelfEventWrapper* wrapper = static_cast<SSTSelfEventWrapper*>(ev);
  wrapper->run();
  delete wrapper;
}

SST::SimTime_t
SSTIntegratedComponent::extra_delay(timestamp t) const
{
  SST::SimTime_t current = getCurrentSimTime(time_converter_);
  SST::SimTime_t timestamp_time = t.ticks_int64();
  return timestamp_time - current;
}

void 
connection_details::parse_src(const std::string& str)
{
  parse_type_id(str, src_type, src_id);
}

void 
connection_details::parse_dst(const std::string& str)
{
  parse_type_id(str, dst_type, dst_id);
}

void
connection_details::parse_type_id(const std::string& str, endpoint_t& ep, int& id)
{
  if     (str.substr(0,4) == "node"){    
    ep = node;
    id = atoi(str.substr(4,5).c_str());
  }
  else if(str.substr(0,6) == "switch"){
    ep = sw;
    id = atoi(str.substr(6,7).c_str());
  }
  else {
    spkt_throw_printf(sprockit::value_error,
      "invalid component %s in link", str.c_str());
  }

}

connection_details
parse_port_name(const std::string& port_name)
{
  connection_details rv;
  // Replace _ with space except within ( ) so we can use an instream to read the tokens
  std::string split = "";
  int in_parens = 0;
  for(auto ch : port_name) {
    if(ch == '(') {
      ++in_parens;
    }
    else {
      if(in_parens) {
        if(ch == ')') --in_parens;
        else split.push_back(ch);
      }
      else {
        if(ch == '_') split.push_back(' ');
        else split.push_back(ch);
      }
    }
  }

  std::istringstream isstr(split);
  std::string read, inout, side;
  isstr >> read; assert(read == "auto");
  isstr >> read; assert(read == "port");
  isstr >> inout; assert(inout == "input" || inout == "output");

  rv.type = inout == "input" ? hw::connectable::input : hw::connectable::output;
  isstr >> read;  // one of the names
  rv.parse_src(read);
  isstr >> rv.src_port;
  isstr >> read; assert(read == "to");
  isstr >> read; // the other name
  rv.parse_dst(read);
  isstr >> rv.dst_port;
  isstr >> rv.weight;
  isstr >> rv.redundancy;

  return rv;
}

}

