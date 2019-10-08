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

fcontext_transfer_t sstmac_jump_fcontext(fcontext_t const to, void * vp);

fcontext_t sstmac_make_fcontext(void * sp, size_t size, pfn_fcontext corofn);

} //end extern C

namespace sstmac {
namespace sw {

class ThreadingFContext : public ThreadContext
{
 private:
  static void start_fcontext_thread(fcontext_transfer_t t)
  {
    ThreadingFContext* fctx = (ThreadingFContext*) t.data;
    fctx->transfer_ = t.ctx;
    (*fctx->fxn_)(fctx->args_);
  }

 public:
  SST_ELI_REGISTER_DERIVED(
    ThreadContext,
    ThreadingFContext,
    "macro",
    "fcontext",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "uses fcontext for fast context switching")

  virtual ~ThreadingFContext() {}

  ThreadingFContext(){}

  ThreadContext* copy() const override {
    //parameters never actually used
    return new ThreadingFContext;
  }

  void initContext() override {}

  void destroyContext() override {}

  void startContext(void *stack, size_t sz,
      void (*func)(void*), void *args,
      ThreadContext* from) override {
    fxn_ = func;
    void* stacktop = (char*) stack + sz;
    ctx_ = sstmac_make_fcontext(stacktop, sz, start_fcontext_thread);
    args_ = args;
    ctx_ = sstmac_jump_fcontext(ctx_, this).ctx;
  }

  void pauseContext(ThreadContext* to) override {
    ThreadingFContext* fctx = static_cast<ThreadingFContext*>(to);
    transfer_ = sstmac_jump_fcontext(transfer_, nullptr).ctx;
  }

  void resumeContext(ThreadContext* from) override {
    auto newctx = sstmac_jump_fcontext(ctx_, nullptr).ctx;
    ctx_ = newctx;
  }

  void completeContext(ThreadContext* to) override {
    sstmac_jump_fcontext(transfer_, nullptr);
  }

  void jumpContext(ThreadContext* to) override {
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

