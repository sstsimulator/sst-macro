#ifndef SSTMAC_LLVM_MEMTRACE_TOOLS_H_INCLUDED
#define SSTMAC_LLVM_MEMTRACE_TOOLS_H_INCLUDED

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Casting.h"
#include <cstdint>

// How we mark lines and functions, it is up to the user to decide what to do if
// we pass back something like Ignore | Memtrace | OpCount.  This is possible
// because a function might be marked ignore, but an instruction inlined into
// that function might be marked Memtrace | OpCount.
enum AnnotationKind {
  None = 1 << 0,
  Ignore = 1 << 1,
  Memtrace = 1 << 2,
  OpCount = 1 << 3
};

class AnnotationMap {
public:
  AnnotationMap() = default;
  AnnotationMap(AnnotationMap const &) = default;

  // Determines if a instruction is in the Map, returns None if it is not
  int matchInst(llvm::Instruction const *I) const;
  
  // Returns the annotation type for the function, if only specific lines in a
  // function are marked for annotations then this returns ignore since we want
  // to ignore non explicitly marked instructions
  AnnotationKind matchFunc(llvm::Function const *F) const;

  void dumpMatches() const;

  void addFunctionAnnotation(llvm::Function const *, AnnotationKind);
  void addSrcLinesAnnotation(llvm::Function const *,
                             llvm::SmallVector<int, 5> &&, AnnotationKind);

private:
  AnnotationKind matchLine(llvm::DILocation const *) const;

  llvm::DenseMap<llvm::Function const *, AnnotationKind> FunctionAnnotations;
  llvm::DenseMap<llvm::DIFile const *, llvm::DenseMap<int, AnnotationKind>>
      LineAnnotations;
};

AnnotationMap parseAnnotations(llvm::Module &M);

// Find functions that need to get picked up that aren't already annotated
void appendRegexFuncMatches(llvm::Module &, AnnotationMap &);

// Declare the functions that we need to call for the SST code and return
// a map to them so that we can use them in passes
llvm::StringMap<llvm::Function *> declareSSTFunctions(llvm::Module &,
                                                      AnnotationKind);

#endif // SSTMAC_LLVM_MEMTRACE_TOOLS_H_INCLUDED
