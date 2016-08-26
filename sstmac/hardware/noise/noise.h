#ifndef sstmac_hardware_noise_NOISE_H
#define sstmac_hardware_noise_NOISE_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/rng.h>

namespace sstmac {
  namespace hw {

class noise_model :
  public sprockit::factory_type
{
 public:

  virtual ~noise_model(){}

  virtual double value() = 0;

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

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

  gaussian_noise_model() : rng_(nullptr) {}

  ~gaussian_noise_model();

  std::string
  to_string() const {
    return "gaussian noise model";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  double value();

 protected:
  RNG::NormalDistribution* rng_;
};

  }
}

#endif // NOISE_H
