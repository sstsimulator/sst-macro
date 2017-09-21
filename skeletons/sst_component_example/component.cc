#include <sstmac/hardware/common/connection.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/sst_event.h>


using namespace sstmac;
using namespace sstmac::hw;

/**
For creating custom hardware components inside of SST/macro, it follows
essentially the same steps as a regular SST component. However, SST/macro
provides an extra wrapper layer and present a slightly different interface
for connecting objects together in the simulated network. This 'connectable'
interface is presented below.
*/


#if SSTMAC_INTEGRATED_SST_CORE
using namespace SST;
/**
No special Python actions are needed so this is null.
*/
char py[] = {0x00};

/**
 * @brief The TestModule class
 * SST will look for this module information after loading libtest.so using dlopen
 * dlopen of libtest.so is triggered by running 'import sst.test'
 * inside the Python configuration script
 */
class TestModule : public SSTElementPythonModule {
 public:
  TestModule(std::string library) :
    SSTElementPythonModule(library)
  {
    addPrimaryModule(py);
  }

  SST_ELI_REGISTER_PYTHON_MODULE(
   TestModule,
   "test",
   SST_ELI_ELEMENT_VERSION(1,0,0)
  )
};
#include <sst/core/model/element_python.h>
#include <sst/core/element.h>
#endif

/**
 * @brief The test_component class
 * For sst-macro, all the componennts must correspond to a parent factory type
 * in this case, we create a factory type test_component from which all
 * test components will inherit
 */
class test_component : public connectable_component {
 public:
  DeclareFactory(test_component,uint64_t,event_manager*)

  /**
   * @brief test_component Standard constructor for all components
   *  with 3 basic parameters
   * @param params  All of the parameters scoped for this component
   * @param id      A unique ID for this component
   * @param mgr     The event manager that will schedule events for this component
   */
  test_component(sprockit::sim_parameters* params, uint32_t id, event_manager* mgr) :
    connectable_component(params,id,mgr)
 {
 }
};

class test_event : public event {
 public:
  //Make sure to satisfy the serializable interface
  ImplementSerializable(test_event)
};

/**
 * @brief The dummy_switch class
 * This is a basic instance of a test component that will generate events
 */
class dummy_switch : public test_component {
 public:
  RegisterComponent("dummy", test_component, dummy_switch,
           "test", COMPONENT_CATEGORY_NETWORK,
           "A dummy switch for teaching")

  /**
   * @brief dummy_switch Standard constructor for all components
   *  with 3 basic parameters
   * @param params  All of the parameters scoped for this component
   * @param id      A unique ID for this component
   * @param mgr     The event manager that will schedule events for this component
   */
  dummy_switch(sprockit::sim_parameters* params, uint32_t id, event_manager* mgr) :
   test_component(params,id,mgr), id_(id)
  {
    //make sure this function gets called
    //unfortunately, due to virtual function initialization order
    //this has to be called in the base child class
    init_links(params);
    //init params
    num_ping_pongs_ = params->get_optional_int_param("num_ping_pongs", 2);
    latency_ = params->get_time_param("latency");
  }

  std::string to_string() const override { return "dummy";}

  void recv_payload(event* ev){
    std::cout << "Oh, hey, component " << id_ << " got a payload!" << std::endl;
    test_event* tev = dynamic_cast<test_event*>(ev);
    if (tev == nullptr){
      std::cerr << "received wrong event type" << std::endl;
      abort();
    }
    if (num_ping_pongs_ > 0){
      send_ping_message();
    }
  }

  void recv_credit(event* ev){
    //ignore for now, we won't do anything with credits
  }

  void connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_link* link) override {
    //register handler on port
    partner_ = link;
    std::cout << "Connecting output "
              << src_outport << "-> " << dst_inport
              << " on component " << id_ << std::endl;
  }

  void connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_link* link) override {
    //we won't do anything with credits, but print for tutorial
    std::cout << "Connecting input "
              << src_outport << "-> " << dst_inport
              << " on component " << id_ << std::endl;
  }

  void setup() override {
    std::cout << "Setting up " << id_ << std::endl;
    //make sure to call parent setup method
    test_component::setup();
    //send an initial test message
    send_ping_message();
  }

  void init(unsigned int phase) override {
    std::cout << "Initializing " << id_
              << " on phase " << phase << std::endl;
    //make sure to call parent init method
    test_component::init(phase);
  }

  link_handler* credit_handler(int port) const override {
    return new_link_handler(this, &dummy_switch::recv_credit);
  }

  link_handler* payload_handler(int port) const override {
    return new_link_handler(this, &dummy_switch::recv_payload);
  }

 private:
  void send_ping_message(){
    partner_->send_delay(latency_, new test_event);
    --num_ping_pongs_;
  }

 private:
  event_link* partner_;
  timestamp latency_;
  int num_ping_pongs_;
  uint32_t id_;

};

