#include <sstmac/hardware/common/connection.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/sst_event.h>


using namespace sstmac;
using namespace sstmac::hw;

/**
For creating custom hardware components inside of SST/macro, it follows
essentially the same steps as a regular SST component. However, SST/macro
provides an extra wrapper layer and present a slightly different interface
for connecting objects together in the simulated network. This 'Connectable'
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
class TestComponent : public ConnectableComponent {
 public:
  DeclareFactory(TestComponent,uint64_t)

  /**
   * @brief test_component Standard constructor for all components
   *  with 3 basic parameters
   * @param params  All of the parameters scoped for this component
   * @param id      A unique ID for this component
   * @param mgr     The event manager that will schedule events for this component
   */
  TestComponent(SST::Params& params, uint32_t id) :
    ConnectableComponent(params,id)
 {
 }
};

class TestEvent : public Event {
 public:
  //Make sure to satisfy the serializable interface
  ImplementSerializable(TestEvent)
};

/**
 * @brief The dummy_switch class
 * This is a basic instance of a test component that will generate events
 */
class DummySwitch : public TestComponent {
 public:
  RegisterComponent("dummy", TestComponent, DummySwitch,
           "test", COMPONENT_CATEGORY_NETWORK,
           "A dummy switch for teaching")

  /**
   * @brief dummy_switch Standard constructor for all components
   *  with 3 basic parameters
   * @param params  All of the parameters scoped for this component
   * @param id      A unique ID for this component
   * @param mgr     The event manager that will schedule events for this component
   */
  DummySwitch(SST::Params& params, uint32_t id) :
   TestComponent(params,id), id_(id)
  {
    //make sure this function gets called
    //unfortunately, due to virtual function initialization order
    //this has to be called in the base child class
    initLinks(params);
    //init params
    num_ping_pongs_ = params->get_optional_int_param("num_ping_pongs", 2);
    latency_ = Timestamp(params->get_time_param("latency"));
  }

  std::string toString() const override { return "dummy";}

  void recvPayload(Event* ev){
    std::cout << "Oh, hey, component " << id_ << " got a payload!" << std::endl;
    TestEvent* tev = dynamic_cast<TestEvent*>(ev);
    if (tev == nullptr){
      std::cerr << "received wrong event type" << std::endl;
      abort();
    }
    if (num_ping_pongs_ > 0){
      sendPingMessage();
    }
  }

  void recvCredit(Event* ev){
    //ignore for now, we won't do anything with credits
  }

  void connectOutput(
    SST::Params& params,
    int src_outport,
    int dst_inport,
    EventLink* link) override {
    //register handler on port
    partner_ = link;
    std::cout << "Connecting output "
              << src_outport << "-> " << dst_inport
              << " on component " << id_ << std::endl;
  }

  void connectInput(
    SST::Params& params,
    int src_outport,
    int dst_inport,
    EventLink* link) override {
    //we won't do anything with credits, but print for tutorial
    std::cout << "Connecting input "
              << src_outport << "-> " << dst_inport
              << " on component " << id_ << std::endl;
  }

  void setup() override {
    std::cout << "Setting up " << id_ << std::endl;
    //make sure to call parent setup method
    TestComponent::setup();
    //send an initial test message
    sendPingMessage();
  }

  void init(unsigned int phase) override {
    std::cout << "Initializing " << id_
              << " on phase " << phase << std::endl;
    //make sure to call parent init method
    TestComponent::init(phase);
  }

  LinkHandler* creditHandler(int port) override {
    return newLinkHandler(this, &DummySwitch::recvCredit);
  }

  LinkHandler* payloadHandler(int port) override {
    return newLinkHandler(this, &DummySwitch::recvPayload);
  }

  Timestamp sendLatency(SST::Params& params) const override {
    return latency_;
  }

  Timestamp creditLatency(SST::Params& params) const override {
    return latency_;
  }

 private:
  void sendPingMessage(){
    partner_->send(new TestEvent);
    --num_ping_pongs_;
  }

 private:
  EventLink* partner_;
  Timestamp latency_;
  int num_ping_pongs_;
  uint32_t id_;

};

