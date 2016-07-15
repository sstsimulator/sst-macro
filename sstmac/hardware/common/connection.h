#ifndef CONNECTION_H
#define CONNECTION_H

#include <sstmac/common/event_scheduler.h>

#define connectable_type_invalid(ty) \
   spkt_throw_printf(sprockit::value_error, "invalid connectable type %s", connectable::str(ty))

#define connect_str_case(x) case x: return #x

namespace sstmac {
namespace hw {


class connectable
{
 public:
  virtual std::string
  to_string() const = 0;

  static const int any_port = -1;

  typedef enum {
    RedundantConnection=0, /*!< The connection has extra redundant links */
    WeightedConnection=1, /*!< The connection is weighted. Weighting applies
                               to bandwidths and buffer sizes */
    FixedBandwidthConnection=2, /*!< The connection has a fixed bandwidth */
    FixedConnection=3, /*!< The connection has a fixex bandwidth and latency */
    BasicConnection=4 /*!< The connection has no special properties
                           Use only the defauly properties */
  } config_type_t;

  struct config {
    config_type_t ty;
    double src_buffer_weight;
    double dst_buffer_weight;
    double xbar_weight;
    double link_weight;
    int red;
    double bw;
    timestamp latency;
    config() : xbar_weight(1.0){}
  };

  typedef enum {
    output,
    input
  } connection_type_t;


  static const char*
  str(connection_type_t ty) {
    switch (ty) {
      connect_str_case(output);
      connect_str_case(input);
    }
  }

  /**
   * @brief connect
   * @param src_outport The outgoing port at the source
   * @param dst_inport The incoming port at the destination
   * @param ty    Whether we are configuring the input or output direction
   * @param mod   The device currently being connected
   * @param cfg   A struct with various special configuration options
   */
  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg) = 0;

};

class connectable_component :
  public event_scheduler,
  public connectable
{
 protected:
#if SSTMAC_INTEGRATED_SST_CORE
  connectable_component(SST::ComponentId_t id,
    SST::Params& params) : event_scheduler(id, params)
  {
  }
#endif
};

class connectable_subcomponent :
  public event_subscheduler,
  public connectable
{
#if SSTMAC_INTEGRATED_SST_CORE
 public:
  virtual void
  init_sst_params(SST::Params& params, SST::Component* parent){}
#endif
};

}

}

#endif // CONNECTION_H

