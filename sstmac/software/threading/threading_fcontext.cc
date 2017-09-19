#include <boost/context/all.hpp>
#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/thread_info.h>
#include <iostream>

namespace sstmac {
namespace sw {

namespace ctx=boost::context;

struct do_nothing_stack_alloc
{
  void deallocate(ctx::stack_context){}
};

class threading_fcontext : public threading_interface
{
 private:
  static void start_context(threading_fcontext* fctx, void (*func)(void*), void* args,
                            void* stack, size_t sz){
    char* stackptr = (char*) stack;

    ctx::stack_context sctx;
    sctx.sp = stackptr + sz;
    sctx.size = sz;

    fctx->resumer_ = ctx::callcc(std::allocator_arg,
      ctx::preallocated(sctx.sp,sz,sctx),
      do_nothing_stack_alloc(),
      [func,args,fctx](ctx::continuation&& sink){
           fctx->pauser_ = std::move(sink);
           func(args);
           return std::move(fctx->pauser_);
         });
  }

 public:
  FactoryRegister("fcontext", threading_interface, threading_fcontext)

  virtual ~threading_fcontext() {}

  threading_fcontext(sprockit::sim_parameters* params) {}

  threading_interface* copy() const override {
    //parameters never actually used
    return new threading_fcontext(nullptr);
  }

  void init_context() override {}

  void destroy_context() override {}

  void start_context(int physical_thread_id,
      void *stack, size_t sz,
      void (*func)(void*), void *args,
      void* globals_storage,
      threading_interface* from) override {
    thread_info::register_user_space_virtual_thread(physical_thread_id, stack, globals_storage);
    start_context(this, func, args, stack, sz);
  }

  void pause_context(threading_interface* to) override {
    pauser_ = pauser_.resume();
  }

  void resume_context(threading_interface* from) override {
    resumer_ = resumer_.resume();
  }

  void complete_context(threading_interface *to) override {
    //no-op, just let the thread run out
  }

 private:
  ctx::continuation resumer_;
  ctx::continuation pauser_;

};

}
}

