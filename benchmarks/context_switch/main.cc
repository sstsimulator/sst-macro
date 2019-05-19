#include <sstmac/main/sstmac.h>
#include <vector>
#include <sstmac/software/threading/threading_interface.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/software/threading/stack_alloc.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "context", "the name of the context switch library to profile" },
{ "nthread", "the number of distinct threads to context switch amongst" },
{ "niter", "the number of context switch iterations to run" },
);

struct subthread_args {
  sstmac::sw::ThreadContext* subthread;
  sstmac::sw::ThreadContext* main_thread;
};

class context_switch_benchmark : public sstmac::Benchmark
{
 public:
  FactoryRegister("context_switch", sstmac::benchmark, context_switch_benchmark);

  context_switch_benchmark(SST::Params& params){
    main_thread_ = sstmac::sw::ThreadContext::factory
                    ::get_param("context", params);
    nthread_ = params.find<int>("nthread");
    niter_ = params.find<int>("niter");
    sstmac::sw::StackAlloc::init(params);
  }

  void run() override;

 private:
  std::vector<subthread_args> subthreads_;
  sstmac::sw::ThreadContext* main_thread_;
  int nthread_;
  int niter_;
};

static void run_subthread(void* args){
  subthread_args* sargs = (subthread_args*) args;
  auto subthread = sargs->subthread;
  auto main_thread = sargs->main_thread;
  while (1){
    subthread->pauseContext(main_thread);
  }
}

void context_switch_benchmark::run()
{
  main_thread_->initContext();
  subthreads_.resize(nthread_);
  for (int i=0; i < nthread_; ++i){
    auto& args = subthreads_[i];
    auto thr = main_thread_->copy();
    args.subthread = thr;
    args.main_thread = main_thread_;
    thr->startContext(0, sstmac::sw::StackAlloc::alloc(),
             sstmac::sw::StackAlloc::stacksize(),
             run_subthread, &args, nullptr, nullptr, main_thread_);
  }

  double start = now();
  for (int i=0; i < niter_; ++i){
    for (int j=0; j < nthread_; ++j){
      sstmac::sw::ThreadContext* subthread = subthreads_[j].subthread;
      subthread->resumeContext(main_thread_);
    }
  }
  double stop = now();
  printf("Benchmark ran for %12.8fs\n", stop - start);
}

