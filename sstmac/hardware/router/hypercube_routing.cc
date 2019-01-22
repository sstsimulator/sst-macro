#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sstmac/hardware/topology/hypercube.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>


namespace sstmac {
namespace hw {

class HypercubeMinimalRouter : public Router {
 public:
  FactoryRegister("hypercube_minimal",
              Router, HypercubeMinimalRouter,
              "router implementing minimal routing for hypercube")

  HypercubeMinimalRouter(SST::Params& params, Topology *top,
                         NetworkSwitch *netsw)
    : Router(params, top, netsw)
  {
    cube_ = safe_cast(Hypercube, top);
    inj_offset_ = 0;
    for (int size : cube_->dimensions()){
      inj_offset_ += size;
    }
  }

  std::string toString() const override {
    return "hypercube minimal router";
  }

  int numVC() const override {
    return 1;
  }

  void route(Packet *pkt) override {
    auto* hdr = pkt->rtrHeader<Packet::header>();
    SwitchId ej_addr = pkt->toaddr() / cube_->concentration();
    if (ej_addr == my_addr_){
      hdr->edge_port = pkt->toaddr() % cube_->concentration() + inj_offset_;
      hdr->deadlock_vc = 0;
      return;
    }

    cube_->minimalRouteToSwitch(my_addr_, ej_addr, hdr);
    hdr->deadlock_vc = 0;
  }

 protected:
  Hypercube* cube_;
  int inj_offset_;
};

class HypercubeParRouter : public HypercubeMinimalRouter {
  struct header : public Packet::header {
    uint8_t stage_number : 3;
    uint8_t nhops : 3;
    uint8_t dstX : 6;
    uint8_t dstY : 6;
    uint8_t dstZ : 6;
    uint16_t ejPort;
    uint32_t dest_switch : 24;
  };

 public:
  static const char initial_stage = 0;
  static const char minimal_stage = 1;
  static const char valiant_stage = 2;
  static const char final_stage = 3;

  FactoryRegister("hypercube_par",
              Router, HypercubeParRouter,
              "router implementing PAR for hypercube/hyperX")

  std::string toString() const override {
    return "hypercube PAR router";
  }

  HypercubeParRouter(SST::Params& params, Topology *top,
                       NetworkSwitch *netsw)
    : HypercubeMinimalRouter(params, top, netsw)
  {
    auto coords = cube_->switchCoords(my_addr_);
    if (coords.size() != 3){
      spkt_abort_printf("PAR routing currently only valid for 3D");
    }
    myX_ = coords[0];
    myY_ = coords[1];
    myZ_ = coords[2];
    auto& dims = cube_->dimensions();
    x_ = dims[0];
    y_ = dims[1];
    z_ = dims[2];
  }

  int x_randomNumber(uint32_t seed){
    int x = myX_;
    uint32_t attempt = 0;
    while (x == myX_){
      x = randomNumber(x_, attempt, seed);
      ++attempt;
    }
    return x;
  }

  int y_randomNumber(uint32_t seed){
    int y = myY_;
    uint32_t attempt = 1; //start 1 to better scramble
    while (y == myY_){
      y = randomNumber(y_, attempt, seed);
      ++attempt;
    }
    return y;
  }

  int z_randomNumber(uint32_t seed){
    int z = myZ_;
    uint32_t attempt = 2; //start 2 to better scramble
    while (z == myZ_){
      z = randomNumber(z_, attempt, seed);
      ++attempt;
    }
    return z;
  }

  void route(Packet *pkt) override {
    auto hdr = pkt->rtrHeader<header>();
    switch(hdr->stage_number){
    case initial_stage: {
      SwitchId ej_addr = pkt->toaddr() / cube_->concentration();
      auto coords = cube_->switchCoords(ej_addr);
      hdr->dstX = coords[0];
      hdr->dstY = coords[1];
      hdr->dstZ = coords[2];
      hdr->ejPort = pkt->toaddr() % cube_->concentration() + inj_offset_;
      hdr->stage_number = minimal_stage;
      hdr->deadlock_vc = 0;
      if (ej_addr == my_addr_){
        hdr->edge_port = hdr->ejPort;
        return;
      }
    }
    case minimal_stage: {
     //have to decide if we want to route valiantly
       uint32_t seed = netsw_->now().time.ticks();
       int minimalPort;
       int valiantPort;
       coordinates coords(3);
       coords[0] = myX_ == hdr->dstX ? myX_ : x_randomNumber(seed);
       coords[1] = myY_ == hdr->dstY ? myY_ : y_randomNumber(seed);
       coords[2] = myZ_ == hdr->dstZ ? myZ_ : z_randomNumber(seed);
       if (myX_ != hdr->dstX){
         minimalPort = hdr->dstX;
         valiantPort = coords[0];
       } else if (myY_ != hdr->dstY){
         minimalPort = hdr->dstY + x_;
         valiantPort = coords[1] + x_;
       } else if (myZ_ != hdr->dstZ){
         minimalPort = hdr->dstZ + x_ + y_;
         valiantPort = coords[2] + x_ + y_;
       } else {
          //oh - um - eject
         hdr->edge_port = hdr->ejPort;
         hdr->deadlock_vc = 0;
         return;
       }
      int minLength = netsw_->queueLength(minimalPort, all_vcs);
      int valLength = netsw_->queueLength(valiantPort, all_vcs) * 2;
      if (minLength <= valLength){
        hdr->edge_port = minimalPort;
      } else {
        SwitchId inter = cube_->switchAddr(coords);
        hdr->edge_port = valiantPort;
        hdr->dest_switch = inter;
        hdr->stage_number = valiant_stage;
      }
      break;
    }
    case valiant_stage: {
      if (my_addr_ != hdr->dest_switch){
        auto coords = cube_->switchCoords(hdr->dest_switch);
        if (coords[0] != myX_){
          hdr->edge_port = coords[0];
        } else if (coords[1] != myY_){
          hdr->edge_port = coords[1]+x_;
        } else {
          hdr->edge_port = coords[2]+x_+y_;
        }
        break;
      }
      hdr->stage_number = final_stage;
      //otherwise fall through
    }
    case final_stage: {
      int port;
      if (hdr->dstX != myX_){
        port = hdr->dstX;
      } else if (hdr->dstY != myY_){
        port = hdr->dstY+x_;
      } else if (hdr->dstZ != myZ_) {
        port = hdr->dstZ+x_+y_;
      } else {
        //oh - um - eject
       hdr->edge_port = hdr->ejPort;
       hdr->deadlock_vc = 0;
       return;
      }
      hdr->edge_port = port;
    }
    }

    hdr->deadlock_vc = hdr->nhops;
    hdr->nhops++;
  }

  int numVC() const override {
    return 6;
  }

 private:
  int myX_;
  int myY_;
  int myZ_;
  int x_;
  int y_;
  int z_;

};

}
}
