#include <sstmac/main/sstmac.h>
#include <vector>
#include <sstmac/software/threading/threading_interface.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/software/threading/stack_alloc.h>

struct subthread_args {
  sstmac::sw::thread_context* subthread;
  sstmac::sw::thread_context* main_thread;
};

class context_switch_benchmark : public sstmac::benchmark
{
 public:
  FactoryRegister("context_switch", sstmac::benchmark, context_switch_benchmark);

  context_switch_benchmark(sprockit::sim_parameters* params){
    main_thread_ = sstmac::sw::thread_context::factory
                    ::get_param("context", params);
    nthread_ = params->get_int_param("nthread");
    niter_ = params->get_int_param("niter");
    sstmac::sw::stack_alloc::init(params);
  }

  void run() override;

 private:
  std::vector<subthread_args> subthreads_;
  sstmac::sw::thread_context* main_thread_;
  int nthread_;
  int niter_;
};

static void run_subthread(void* args){
  subthread_args* sargs = (subthread_args*) args;
  auto subthread = sargs->subthread;
  auto main_thread = sargs->main_thread;
  while (1){
    subthread->pause_context(main_thread);
  }
}

void context_switch_benchmark::run()
{
  main_thread_->init_context();
  subthreads_.resize(nthread_);
  for (int i=0; i < nthread_; ++i){
    auto& args = subthreads_[i];
    auto thr = main_thread_->copy();
    args.subthread = thr;
    args.main_thread = main_thread_;
    thr->start_context(0, sstmac::sw::stack_alloc::alloc(),
             sstmac::sw::stack_alloc::stacksize(),
             run_subthread, &args, nullptr, main_thread_);
  }

  double start = now();
  for (int i=0; i < niter_; ++i){
    for (int j=0; j < nthread_; ++j){
      sstmac::sw::thread_context* subthread = subthreads_[j].subthread;
      subthread->resume_context(main_thread_);
    }
  }
  double stop = now();
  printf("Benchmark ran for %12.8fs\n", stop - start);
}

