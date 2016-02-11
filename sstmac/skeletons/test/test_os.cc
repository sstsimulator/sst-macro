#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

class test_thread : public thread
{

 public:
  test_thread(int threadnum)
    : threadnum_(threadnum)
  {
  }

  void run(){
    std::cout << "Running thread " << threadnum_ << "\n";
  }

 private:
  int threadnum_;

};

class test_os : public app
{
 public:
  test_os(){}

  virtual ~test_os()
  {
  }

  virtual void
  skeleton_main()
  {
    for (int i=0; i < nthread_; ++i) {
      thread* thr = new test_thread(i);
      this->os()->start_thread(thr);
    }
  }

  virtual app*
  clone_type() {
    return new test_os;
  }

  virtual void
  consume_params(sprockit::sim_parameters* params){
    nthread_ = params->get_int_param("test_os_nthread");
  }

  virtual std::string
  to_string() const {
    return "test_os";
  }

 protected:
  int nthread_;

};

SpktRegister("sstmac_test_os", app, test_os,
            "test case for certain os features such as spawing threads");

}
} //end of namespace sstmac

