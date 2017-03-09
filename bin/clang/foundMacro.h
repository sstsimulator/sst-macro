#include "util.h"
#include <list>
#include <iostream>

struct FoundMacro {
  clang::Stmt* stmt;
  clang::SourceLocation start;
  clang::SourceLocation end;
  clang::CompilerInstance& CI;
  bool compound;

  clang::SourceRange
  range() const {
    return clang::SourceRange(start, end);
  }

  FoundMacro(clang::CompilerInstance& c, clang::SourceLocation loc): CI(c), compound(false) {
    //need to figure out start and end of macro
    auto expansionRange = CI.getSourceManager().getExpansionRange(loc);
    if (expansionRange.first == expansionRange.second){
      //okay, this is a macro token, not a macro expansion of the form macro(x,y)
      expansionRange.second = clang::Lexer::getLocForEndOfToken(expansionRange.second, 1,
                                       CI.getSourceManager(), CI.getLangOpts());
    }
    start = expansionRange.first;
    end = expansionRange.second;
  }

  bool overlaps(clang::Stmt* s) const {
    clang::SourceManager& SM = CI.getSourceManager();
    clang::SourceLocation startLoc = SM.getFileLoc(s->getLocStart());
    clang::SourceLocation endLoc = SM.getFileLoc(s->getLocEnd());
    return start <= startLoc && endLoc <= end;
  }

};

struct MacroList {
  std::list<FoundMacro> macros;
  clang::CompilerInstance* CI;

  void
  setCompilerInstance(clang::CompilerInstance& c){
    CI = &c;
  }

  FoundMacro& newMacro(clang::Stmt* s){
    macros.emplace_back(*CI, s->getLocStart());
    macros.back().stmt = s;
    return macros.back();
  }

  bool hasOverlappingMacro(clang::Stmt* s){
    if (macros.empty()) return false;
    FoundMacro& back = macros.back();
    return back.overlaps(s);
  }

  FoundMacro& getOverlappingMacro(clang::Stmt* s){
    clang::SourceManager& SM = CI->getSourceManager();
    if (macros.empty()){
      return newMacro(s);
    } else {
      FoundMacro& back = macros.back();
      if (back.overlaps(s)){
        back.compound = true;
        return back;
      } else {
        return newMacro(s);
      }
    }
  }


};
