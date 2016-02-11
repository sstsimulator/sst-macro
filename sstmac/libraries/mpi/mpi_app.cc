#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/mpi_api.h>

namespace sstmac {
namespace sw {

mpi_app::~mpi_app()
{
}

void
mpi_app::init_factory_params(sprockit::sim_parameters *params)
{
  app::init_factory_params(params);
}

void
mpi_app::init_os(operating_system* os)
{
  app::init_os(os);
  mpi_ = get_api<mpi_api>();
}

void
mpi_app::kill()
{
  app::kill();
}

}
}

