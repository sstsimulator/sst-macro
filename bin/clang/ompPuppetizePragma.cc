#include "ompPuppetizePragma.h"
#include "pragmas.h"

namespace {
struct OmpPuppetInfo {
  annotate::ToolInfo operator()(
      clang::SourceLocation Loc, clang::CompilerInstance &CI,
      std::map<std::string, std::list<std::string>> &&PragmaStrings) const {
    return {"memtrace", {"some", "omp", "args"}};
  }
};
} // namespace

// static PragmaRegister<SSTArgMapPragmaShim, SSTOmpPuppetPragma, false>
//     ompPuppetPragma("omp", "parallel", pragmas::PUPPETIZE);
// 
// static PragmaRegister<SSTArgMapPragmaShim, SSTOmpPuppetPragma, true>
//     ompPuppetPragmaShadow("omp", "parallel", pragmas::SHADOWIZE);
// 
// static PragmaRegister<SSTArgMapPragmaShim, SSTOmpPuppetPragma, false>
//     ompPuppetPragmaMemoize("omp", "parallel", pragmas::MEMOIZE);
