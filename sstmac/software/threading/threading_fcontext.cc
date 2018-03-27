#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/thread_info.h>
#include <iostream>

#include <stdint.h>
#include <stddef.h>

extern "C" {

typedef void* fcontext_t;

struct fcontext_transfer_t
{
    fcontext_t ctx;
    void* data;
};

struct fcontext_stack_t
{
    void* sptr;
    size_t ssize;
};

typedef void (*pfn_fcontext)(fcontext_transfer_t);

fcontext_transfer_t jump_fcontext(fcontext_t const to, void * vp);

fcontext_t make_fcontext(void * sp, size_t size, pfn_fcontext corofn);

} //end extern C

namespace sstmac {
namespace sw {

class threading_fcontext : public thread_context
{
 private:
  static void start_fcontext_thread(fcontext_transfer_t t)
  {
    threading_fcontext* fctx = (threading_fcontext*) t.data; 
    fctx->transfer_ = t.ctx;
    (*fctx->fxn_)(fctx->args_);
  }

 public:
  FactoryRegister("fcontext", thread_context, threading_fcontext)

  virtual ~threading_fcontext() {}

  threading_fcontext(sprockit::sim_parameters* params){}

  thread_context* copy() const override {
    //parameters never actually used
    return new threading_fcontext(nullptr);
  }

  void init_context() override {}

  void destroy_context() override {}

  void start_context(int physical_thread_id,
      void *stack, size_t sz,
      void (*func)(void*), void *args,
      void* globals_storage,
      void* tls_storage,
      thread_context* from) override {
    thread_info::register_user_space_virtual_thread(physical_thread_id, stack,
                                                    globals_storage, tls_storage);
    fxn_ = func;
    void* stacktop = (char*) stack + sz;
    ctx_ = make_fcontext(stacktop, sz, start_fcontext_thread);
    args_ = args;
    ctx_ = jump_fcontext(ctx_, this).ctx;
  }

  void pause_context(thread_context* to) override {
    threading_fcontext* fctx = static_cast<threading_fcontext*>(to);
    transfer_ = jump_fcontext(transfer_, nullptr).ctx;
  }

  void resume_context(thread_context* from) override {
    auto newctx = jump_fcontext(ctx_, nullptr).ctx;
    ctx_ = newctx;
  }

  void complete_context(thread_context* to) override {
    jump_fcontext(transfer_, nullptr);
  }

  void jump_context(thread_context* to) override {
    spkt_abort_printf("error: fcontext interface does not support jump_context feature\n"
                      "must set SSTMAC_THREADING=pth or ucontext");
  }

 private:
  fcontext_t ctx_;
  void* args_;
  void (*fxn_)(void*);
  fcontext_t transfer_;

};

}
}

