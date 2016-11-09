#ifndef sstmac_hardware_noise_NOISE_H
#define sstmac_hardware_noise_NOISE_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/rng.h>

namespace sstmac {
  namespace hw {

class noise_model
{
 public:
  virtual ~noise_model(){}

  virtual double value() = 0;

 protected:
  noise_model(sprockit::sim_parameters* params){}
  noise_model(){}

};
DeclareFactory(noise_model);

class gaussian_noise_model :
  public noise_model
{
 public:
  gaussian_noise_model(
    double mean,
    double stdev,
    double maxz,
    int seed);

  gaussian_noise_model(sprockit::sim_parameters* params);

  ~gaussian_noise_model();

  double value();

 protected:
  RNG::NormalDistribution* rng_;
};

  }
}

#endif // NOISE_H
