#include <mpi.h>
#include <sstmac/util.h>

namespace sendrecv
{

  class sendrecv_skeleton : public sstmac::sw::mpi_app
  {
  public:

  private:
    int message_size_;

  public:
    /** Destructor - do nothing */
    virtual
    ~sendrecv_skeleton() throw ()
    {
    }

    sendrecv_skeleton(){}

    app*
    clone_type()
    {
      return new sendrecv_skeleton;
    }

    void
    consume_params(sprockit::sim_parameters* params)
    {
      message_size_ = params->get_int_param("sendrecv_message_size");
    }

    std::string
    to_string() const
    {
      return "sendrecv";
    }

    virtual void
    skeleton_main();

  };

} //end namespace sendrecv


