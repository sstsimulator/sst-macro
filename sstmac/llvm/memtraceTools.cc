#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
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
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <algorithm>
#include <sstream>

#include "memtraceTools.h"

using namespace llvm;

void AnnotationMap::addFunctionAnnotation(Function const *F, AnnotationKind K) {
  auto Iter = FunctionAnnotations.find(F);
  if (Iter == FunctionAnnotations.end()) {
    FunctionAnnotations.insert({F, K});
  } else {
    if (Iter->second != K) {
      report_fatal_error(
          "Trying to set a function annotation with different AnnotationKinds");
    }
  }
}

void AnnotationMap::addSrcLinesAnnotation(Function const *F,
                                          SmallVector<int, 5> &&Lines,
                                          AnnotationKind K) {

  auto File = F->getSubprogram()->getFile();
  auto &LineMap = LineAnnotations[File];
  for (auto const &L : Lines) {
    LineMap[L] = K; // Don't check for collisions right now
  }
}

AnnotationKind AnnotationMap::matchLine(llvm::DILocation const *Dl) const {
  if (!Dl) { // Check that our DebugLocation is valid
    return AnnotationKind::None;
  }

  // If no matching file return None
  auto const *File = Dl->getFile();
  auto FileIter = LineAnnotations.find(File);
  if (FileIter == LineAnnotations.end()) {
    return AnnotationKind::None;
  }

  auto const &AKLineMap = FileIter->second;
  auto LineIter = AKLineMap.find(Dl->getLine());
  if (LineIter == AKLineMap.end()) {
    return AnnotationKind::None;
  }

  return LineIter->second;
}

AnnotationKind AnnotationMap::matchFunc(llvm::Function const *F) const {
  auto FuncIter = FunctionAnnotations.find(F);
  if (FuncIter == FunctionAnnotations.end()) {
    return AnnotationKind::None;
  }

  return FuncIter->second;
}

void AnnotationMap::dumpMatches() const {
  errs() << "Annotated functions:\n";
  for (auto const &Pair : FunctionAnnotations) {
    errs() << "\t" << Pair.first->getName() << ": " << Pair.second << "\n";
  }

  errs() << "Annotated files and lines {Line,Annotation}:\n";
  for (auto const &Pair : LineAnnotations) {
    errs() << "\t" << Pair.first << "=" << Pair.first->getName() << ": ";
    for (auto const &Pair2 : Pair.second) {
      errs() << "{" << Pair2.first << "," << Pair2.second << "} ";
    }
    errs() << "\n";
  }
}

int AnnotationMap::matchInst(llvm::Instruction const *I) const {
  auto FuncAK = matchFunc(I->getFunction());
  auto LinesAK = matchLine(I->getDebugLoc());

  return FuncAK | LinesAK;
}

StringMap<Function *> declareSSTFunctions(Module &M, AnnotationKind K) {
  StringMap<Function *> Funcs;

  // Types needed
  auto IntPtrType8 = Type::getInt8PtrTy(M.getContext());
  auto IntType32 = Type::getInt32Ty(M.getContext());
  auto IntType64 = Type::getInt64Ty(M.getContext());
  auto VoidType = Type::getVoidTy(M.getContext());

  // Functions we always need
  {
    auto StartType = FunctionType::get(VoidType, false);
    Funcs["start_trace"] = Function::Create(
        StartType, Function::ExternalLinkage, "sstmac_start_trace", M);

    auto StopType = StartType;
    Funcs["stop_trace"] = Function::Create(StopType, Function::ExternalLinkage,
                                           "sstmac_end_trace", M);

    // TODO Disable this if we are not using OMP
    auto ThreadNumType = FunctionType::get(IntType32, false);
    Funcs["omp_get_thread_num"] = Function::Create(
        ThreadNumType, Function::ExternalLinkage, "omp_get_thread_num", M);
  }

  if (K == AnnotationKind::Memtrace) {
    auto LoadType =
        FunctionType::get(VoidType, {IntPtrType8, IntType64, IntType32}, false);
    Funcs["Load"] = Function::Create(LoadType, Function::ExternalLinkage,
                                     "sstmac_address_load", M);

    auto StoreType = LoadType;
    Funcs["Store"] = Function::Create(StoreType, Function::ExternalLinkage,
                                      "sstmac_address_store", M);

    auto DumpType = FunctionType::get(VoidType, false);
    Funcs["Dump"] = Function::Create(DumpType, Function::ExternalLinkage,
                                     "sstmac_print_address_info", M);
  }

  return Funcs;
}

namespace {
SmallVector<int, 5> getLines(StringRef const &S) {
  auto NumStart = S.find_first_of("{") + 1;
  auto NumEnd = S.find_first_of("}");
  auto Temp = StringRef(S.data() + NumStart, NumEnd - NumStart);

  SmallVector<StringRef, 5> Nums;
  Temp.split(Nums, ",");

  SmallVector<int, 5> Out;
  for (auto const &Num : Nums) {
    Out.push_back(std::stoi(Num));
  }

  return Out;
}

SmallString<10> getAnnotation(ConstantStruct *CS) {
  auto AnnotationGL =
      dyn_cast<GlobalVariable>(CS->getOperand(1)->getOperand(0));
  return dyn_cast<ConstantDataArray>(AnnotationGL->getInitializer())
      ->getAsCString();
}
} // namespace

AnnotationMap parseAnnotations(Module &M) {
  AnnotationMap MyMap;
  for (auto const &I : M.globals()) {
    if (I.getName() == "llvm.global.annotations") {
      ConstantArray *CA = dyn_cast<ConstantArray>(I.getOperand(0));

      for (auto OI = CA->op_begin(), End = CA->op_end(); OI != End; ++OI) {
        auto CS = dyn_cast<ConstantStruct>(OI->get());
        auto Func = dyn_cast<Function>(CS->getOperand(0)->getOperand(0));
        auto Annotation = getAnnotation(CS);

        if (Annotation.count("memtrace:{")) {
          MyMap.addFunctionAnnotation(Func, AnnotationKind::Ignore);
          MyMap.addSrcLinesAnnotation(Func, getLines(Annotation),
                                      AnnotationKind::Memtrace);
        } else if (Annotation.count("memtrace:ignore,{")) { // Ignore some
          MyMap.addFunctionAnnotation(Func, AnnotationKind::Ignore);
          MyMap.addSrcLinesAnnotation(Func, getLines(Annotation),
                                      AnnotationKind::Ignore);
        } else if (Annotation.count("memtrace:ignore")) {
          MyMap.addFunctionAnnotation(Func, AnnotationKind::Ignore);
        } else if (Annotation.count("memtrace:all,{")) {
          MyMap.addFunctionAnnotation(Func, AnnotationKind::Memtrace);
          MyMap.addSrcLinesAnnotation(Func, getLines(Annotation),
                                      AnnotationKind::Memtrace);
        } // Doesn't check for inlined instructions
        else if (Annotation.count("memtrace:all")) {
          MyMap.addFunctionAnnotation(Func, AnnotationKind::Memtrace);
        }
      }
    }
  }

  return MyMap;
}

// Find functions that need to get picked up that aren't annotated
void appendRegexFuncMatches(llvm::Module &M, AnnotationMap &AM) {
  const StringMap<AnnotationKind> AlwaysMatch = {
      {"omp", AnnotationKind::Memtrace}};

  for (auto const &F : M) {
    auto const &Name = F.getName();
    for (auto const &Match : AlwaysMatch) {
      if (Name.contains(Match.getKey())) {
        AM.addFunctionAnnotation(&F, Match.second);
      }
    }
  }
}
