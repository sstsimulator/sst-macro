#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/dragonfly_plus.h>
#include <sstmac/hardware/topology/hypercube.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>


namespace sstmac {
namespace hw {

class hypercube_minimal_router : public router {
 public:
  FactoryRegister("hypercube_minimal",
              router, hypercube_minimal_router,
              "router implementing minimal routing for hypercube")

  hypercube_minimal_router(sprockit::sim_parameters* params, topology *top,
                         network_switch *netsw)
    : router(params, top, netsw)
  {
    cube_ = safe_cast(hypercube, top);
    inj_offset_ = 0;
    for (int size : cube_->dimensions()){
      inj_offset_ += size;
    }
  }

  std::string to_string() const override {
    return "hypercube minimal router";
  }

  int num_vc() const override {
    return 1;
  }

  void route(packet *pkt) override {
    auto* hdr = pkt->rtr_header<packet::header>();
    switch_id ej_addr = pkt->toaddr() / cube_->concentration();
    if (ej_addr == my_addr_){
      hdr->port = pkt->toaddr() % cube_->concentration() + inj_offset_;
      hdr->vc = 0;
      return;
    }

    cube_->minimal_route_to_switch(my_addr_, ej_addr, hdr);
    hdr->vc = 0;
  }

 protected:
  hypercube* cube_;
  int inj_offset_;
};

class hypercube_par_router : public hypercube_minimal_router {
  struct header : public packet::header {
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
              router, hypercube_par_router,
              "router implementing PAR for hypercube/hyperX")

  std::string to_string() const override {
    return "hypercube PAR router";
  }

  hypercube_par_router(sprockit::sim_parameters* params, topology *top,
                       network_switch *netsw)
    : hypercube_minimal_router(params, top, netsw)
  {
    auto coords = cube_->switch_coords(my_addr_);
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

  int x_random_number(uint32_t seed){
    int x = myX_;
    uint32_t attempt = 0;
    while (x == myX_){
      x = random_number(x_, attempt, seed);
      ++attempt;
    }
    return x;
  }

  int y_random_number(uint32_t seed){
    int y = myY_;
    uint32_t attempt = 1; //start 1 to better scramble
    while (y == myY_){
      y = random_number(y_, attempt, seed);
      ++attempt;
    }
    return y;
  }

  int z_random_number(uint32_t seed){
    int z = myZ_;
    uint32_t attempt = 2; //start 2 to better scramble
    while (z == myZ_){
      z = random_number(z_, attempt, seed);
      ++attempt;
    }
    return z;
  }

  void route(packet *pkt) override {
    auto hdr = pkt->rtr_header<header>();
    switch(hdr->stage_number){
    case initial_stage: {
      switch_id ej_addr = pkt->toaddr() / cube_->concentration();
      auto coords = cube_->switch_coords(ej_addr);
      hdr->dstX = coords[0];
      hdr->dstY = coords[1];
      hdr->dstZ = coords[2];
      hdr->ejPort = pkt->toaddr() % cube_->concentration() + inj_offset_;
      hdr->stage_number = minimal_stage;
      hdr->vc = 0;
      if (ej_addr == my_addr_){
        hdr->port = hdr->ejPort;
        return;
      }
    }
    case minimal_stage: {
     //have to decide if we want to route valiantly
       uint32_t seed = netsw_->now().ticks();
       int minimalPort;
       int valiantPort;
       coordinates coords(3);
       coords[0] = myX_ == hdr->dstX ? myX_ : x_random_number(seed);
       coords[1] = myY_ == hdr->dstY ? myY_ : y_random_number(seed);
       coords[2] = myZ_ == hdr->dstZ ? myZ_ : z_random_number(seed);
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
         hdr->port = hdr->ejPort;
         hdr->vc = 0;
         return;
       }
      int minLength = netsw_->queue_length(minimalPort);
      int valLength = netsw_->queue_length(valiantPort) * 2;
      if (minLength <= valLength){
        hdr->port = minimalPort;
      } else {
        switch_id inter = cube_->switch_addr(coords);
        hdr->port = valiantPort;
        hdr->dest_switch = inter;
        hdr->stage_number = valiant_stage;
      }
      break;
    }
    case valiant_stage: {
      if (my_addr_ != hdr->dest_switch){
        auto coords = cube_->switch_coords(hdr->dest_switch);
        if (coords[0] != myX_){
          hdr->port = coords[0];
        } else if (coords[1] != myY_){
          hdr->port = coords[1]+x_;
        } else {
          hdr->port = coords[2]+x_+y_;
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
       hdr->port = hdr->ejPort;
       hdr->vc = 0;
       return;
      }
      hdr->port = port;
    }
    }

    hdr->vc = hdr->nhops;
    hdr->nhops++;
  }

  int num_vc() const override {
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
