#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Linker/Linker.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <iostream>

struct MemPass : public llvm::ModulePass {

  static char ID;

  MemPass() : llvm::ModulePass(ID) {}

  ~MemPass(){}

  bool runOnModule(llvm::Module &M) override;

 private:
  llvm::StringRef TargetName_;
};

char MemPass::ID = 0;

bool
MemPass::runOnModule(llvm::Module &M)
{
#if 0
  std::cout << "Running on module " << M.getName().str() << std::endl;

  for (llvm::Function& f : M.functions()){
    std::cout << "Have function " << f.getName().str() << std::endl;
    //for (const llvm::Attribute& attr : f.getAttributes().getFnAttributes()){
    //  std::cout << " attribte=" << attr.getAsString() << std::endl;
    //}
  }
#endif
  return false;
}

static llvm::RegisterPass<MemPass> myPass("sst-memtrace", "Add memory tracing");

static void registerMemTracePass(const llvm::PassManagerBuilder& pass_mgr_builder,
                                 llvm::legacy::PassManagerBase& pass_mgr) {
  if (pass_mgr_builder.OptLevel > 0)
    pass_mgr.add(new MemPass);
}
static llvm::RegisterStandardPasses
RegisterMemTrace_opt(llvm::PassManagerBuilder::EP_ScalarOptimizerLate, registerMemTracePass);

static void registerMemTracePass_O0(const llvm::PassManagerBuilder& pass_mgr_builder,
                                    llvm::legacy::PassManagerBase& pass_mgr) {
  if (pass_mgr_builder.OptLevel == 0)
    pass_mgr.add(new MemPass);
}
static llvm::RegisterStandardPasses
RegisterMemTracePass_O0(llvm::PassManagerBuilder::EP_EnabledOnOptLevel0, registerMemTracePass_O0);

