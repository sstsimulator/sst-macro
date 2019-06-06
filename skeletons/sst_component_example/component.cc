#include <sstmac/hardware/common/connection.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/sst_event.h>
#include <sprockit/sim_parameters.h>


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
  SST_ELI_DECLARE_BASE(TestComponent)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(uint32_t,SST::Params&)
  /**
   * @brief test_component Standard constructor for all components
   *  with 3 basic parameters
   * @param params  All of the parameters scoped for this component
   * @param id      A unique ID for this component
   * @param mgr     The event manager that will schedule events for this component
   */
  TestComponent(uint32_t id, SST::Params& params) :
    ConnectableComponent(id, params)
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
  SST_ELI_REGISTER_DERIVED_COMPONENT(
    TestComponent,
    DummySwitch,
    "macro",
    "dummy",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A dummy switch",
    COMPONENT_CATEGORY_NETWORK)

  /**
   * @brief dummy_switch Standard constructor for all components
   *  with 3 basic parameters
   * @param params  All of the parameters scoped for this component
   * @param id      A unique ID for this component
   * @param mgr     The event manager that will schedule events for this component
   */
  DummySwitch(uint32_t id, SST::Params& params) :
   TestComponent(id,params), id_(id)
  {
    //make sure this function gets called
    //unfortunately, due to virtual function initialization order
    //this has to be called in the base child class
    initLinks(params);
    //init params
    num_ping_pongs_ = params.find<int>("num_ping_pongs", 2);
    latency_ = TimeDelta(params.find<SST::UnitAlgebra>("latency").getValue().toDouble());
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

  void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override {
    //register handler on port
    partner_ = std::move(link);
    std::cout << "Connecting output "
              << src_outport << "-> " << dst_inport
              << " on component " << id_ << std::endl;
  }

  void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override {
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

 private:
  void sendPingMessage(){
    partner_->send(new TestEvent);
    --num_ping_pongs_;
  }

 private:
  EventLink::ptr partner_;
  TimeDelta latency_;
  int num_ping_pongs_;
  uint32_t id_;

};

