#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include <algorithm>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "memtraceTools.h"

using namespace llvm;

namespace {
StringSet<> FunctionCallWhiteList = {
    "gettimeofday",
    "printf"
};

bool functionIsWhiteListed(StringRef const &Str) {
  if (Str.startswith("__kmpc")) { // Whitelist builtin OMP Funcs
    return true;
  }
  return FunctionCallWhiteList.count(Str);
}

// Checks if I is a vector
uint64_t getVectorSize(Instruction const *I) {
  if (auto VT = llvm::dyn_cast<llvm::VectorType>(I->getType())) {
    return VT->getNumElements();
  } else {
    return 1;
  }
}

// Determines the number of bytes loaded or stored by AtomicCmpXchgInst,
// LoadInst, and StoreInst
template <typename LoadStore> Value *getByteCount(LoadStore const *LS) {
  auto DL = LS->getFunction()->getParent()->getDataLayout();
  auto Ptr = LS->getPointerOperand();
  auto SizeInBytes = DL.getTypeStoreSize(Ptr->getType()) * getVectorSize(LS);
  return ConstantInt::get(LS->getContext(), APInt(64, SizeInBytes, false));
}
Value *getByteCount(MemIntrinsic const *MI) { return MI->getLength(); }

// Casts a pointer Value to an int8* which appears to be how llvm passes void*
BitCastInst *toI8Ptr(Value *V, Instruction *InsertBefore) {
  assert(V->getType()->isPointerTy() && "V needs to be a pointer");
  return new BitCastInst(V, Type::getInt8PtrTy(V->getContext()), "",
                         InsertBefore);
}

// This function will bit cast the target address to an int8* then determine how
// many bytes the load/store represents and finally pass that info into an SST
// backend function
template <typename InstType>
void handleMemOp(Value *TargetAddress, InstType *InsertBefore, Value *ThreadID,
                 Function *SSTFunc) {
  auto MemAddress = toI8Ptr(TargetAddress, InsertBefore);
  ArrayRef<Value *> Args = {MemAddress, getByteCount(InsertBefore), ThreadID};
  CallInst::Create(SSTFunc->getFunctionType(), SSTFunc, Args, "", InsertBefore);
}

struct MemtracePass : public ModulePass {
  AnnotationMap Annotations;
  StringMap<Function *> SSTFunctions;
  static char ID;

  MemtracePass() : ModulePass(ID) {}

  SmallVector<Instruction *, 10> getTaggedInsts(Function &F) {
    SmallVector<Instruction *, 10> Insts;
    for (auto &I : instructions(F)) {
      if (I.mayReadOrWriteMemory() &&
          (Annotations.matchInst(&I) & AnnotationKind::Memtrace)) {
        Insts.push_back(&I);
      }
    }
    return Insts;
  }

  void appendReturnInsts(Function &F, SmallVector<Instruction *, 10> &Insts) {
    for (auto &I : instructions(F)) {
      if (I.getOpcode() == Instruction::Ret) {
        Insts.push_back(&I);
      }
    }
  }

  void handleMemoryInst(LoadInst *LD, Value *ThreadID) {
    handleMemOp(LD->getPointerOperand(), LD, ThreadID, SSTFunctions["Load"]);
  }

  void handleMemoryInst(StoreInst *SD, Value *ThreadID) {
    handleMemOp(SD->getPointerOperand(), SD, ThreadID, SSTFunctions["Store"]);
  }

  // MemIntrinsic (memset, memcpy, etc) might store only or might do both
  void handleMemoryInst(MemIntrinsic *Mi, Value *ThreadID) {
    handleMemOp(Mi->getDest(), Mi, ThreadID, SSTFunctions["Store"]); // Store
    if (auto Mt = dyn_cast<MemTransferInst>(Mi)) { // If transfer then Load
      handleMemOp(Mt->getSource(), Mi, ThreadID, SSTFunctions["Load"]);
    }
  }

  // TODO try to capture the store from the atomic also
  void handleMemoryInst(AtomicCmpXchgInst *ACX, Value *ThreadID) {
    auto ExchangeValue = ACX->getNextNonDebugInstruction();
    auto ExchangeSucces = ExchangeValue->getNextNonDebugInstruction();
    // We want to use this instruction to insert before since it is the first
    // once after the check of the atomic condition
    auto PostAtomicInst = ExchangeSucces->getNextNonDebugInstruction();

    auto OpSize = getByteCount(ACX);
    auto Address = toI8Ptr(ACX->getPointerOperand(), PostAtomicInst);
    auto LoadFunc = SSTFunctions["Load"];
    CallInst::Create(LoadFunc->getFunctionType(), LoadFunc,
                     {Address, OpSize, ThreadID}, "", PostAtomicInst);
  }

  // Deals with things like _Znam and other special functions
  bool handleSpecialFunctions(CallSite CS, Value* ThreadID){
    auto Callee = CS.getCalledFunction();
    if(Callee->getName() == "_Znam"){
      // Don't handle _Znam for now since we don't know how many addresses were
      // actually touched.
      return true;
    }

    return false;
  }

  // Handles both CallInst and InvokeInst
  void handleMemCall(CallSite CS, Value *ThreadID) {
    Function const *TargetFunc = CS.getCalledFunction();

    // Most intrinsics we don't care about but we do care about the memory ones
    if (TargetFunc->isIntrinsic()) {
      if (auto MI = dyn_cast<MemIntrinsic>(CS.getInstruction())) {
        handleMemoryInst(MI, ThreadID);
      }
      return;
    }
    
    if(handleSpecialFunctions(CS, ThreadID)){
      return;
    }

    if (functionIsWhiteListed(TargetFunc->getName())) {
      return;
    }

    // If the function is annotated then ignore
    if (Annotations.matchFunc(TargetFunc) != AnnotationKind::None) {
      return;
    }

    std::string CurrentFunction = CS.getInstruction()->getFunction()->getName();
    std::string CalledFunction = TargetFunc->getName();

    std::stringstream error;
    error << "Function(" << CalledFunction
          << ") was not instrumented. Called "
             "from: "
          << CurrentFunction << " either mark it or add it to the whitelist\n";

    report_fatal_error(error.str());
  }

  // Ruturns a value that tells us which thread we are on.
  Value *startTracing(Function *F) {
    auto FirstInst = &F->front().front();

    // TODO Check that OMP is actually enabled otherwise return 0
    auto OmpNumThreads = SSTFunctions["omp_get_thread_num"];
    auto ThreadID = CallInst::Create(OmpNumThreads->getFunctionType(),
                                     OmpNumThreads, "", FirstInst);

    auto StartTracing = SSTFunctions["start_trace"];
    CallInst::Create(StartTracing->getFunctionType(), StartTracing, "",
                     FirstInst);

    return ThreadID;
  }

  void stopTracing(Instruction *Ret) {
    auto StopTracing = SSTFunctions["stop_trace"];
    CallInst::Create(StopTracing->getFunctionType(), StopTracing, "", Ret);
  }

  void runOnFunction(Function &F) {
    auto taggedInsts = getTaggedInsts(F);
    if (taggedInsts.empty()) {
      return;
    } else {
      appendReturnInsts(F, taggedInsts);
    }

    auto ThreadID = startTracing(&F);
    for (auto I : taggedInsts) {
      switch (I->getOpcode()) {
      case Instruction::Load:
        handleMemoryInst(dyn_cast<LoadInst>(I), ThreadID);
        break;
      case Instruction::Store:
        handleMemoryInst(dyn_cast<StoreInst>(I), ThreadID);
        break;
      case Instruction::AtomicCmpXchg:
        handleMemoryInst(dyn_cast<AtomicCmpXchgInst>(I), ThreadID);
        break;
      case Instruction::Call:
        handleMemCall(CallSite(dyn_cast<CallInst>(I)), ThreadID);
        break;
      case Instruction::Invoke:
        handleMemCall(CallSite(dyn_cast<InvokeInst>(I)), ThreadID);
        break;
      case Instruction::Ret:
        stopTracing(I);
        break;
      default:
        errs() << "\n" << *I << " " << I->getOpcodeName() << "\n";
        llvm_unreachable("Unhandled MemoryOp\n");
      }
    }
  }

  bool runOnModule(Module &M) override {
    Annotations = parseAnnotations(M);
    appendRegexFuncMatches(M, Annotations);
    Annotations.dumpMatches();
    SSTFunctions = declareSSTFunctions(M, AnnotationKind::Memtrace);

    for (Function &F : M.functions()) {
      runOnFunction(F);

      // Hack in a function call at the end of main for a bit
      if (F.getName() == "main") {
        for (auto &I : instructions(F)) {
          if (auto Ret = dyn_cast<ReturnInst>(&I)) {
            auto Dump = SSTFunctions["Dump"];
            CallInst::Create(Dump->getFunctionType(), Dump, "", Ret);
          }
        }
      }
    }

    return true;
  }
};

} // namespace

char MemtracePass::ID = 0;
static RegisterPass<MemtracePass> X("sst-memtrace", "SSTMAC Memtrace Pass",
                                    false, false);
