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

  typedef std::pair<clang::SourceLocation,clang::SourceLocation> LocPair;

  void getLocations(clang::SourceLocation loc, LocPair& pair){
    pair = CI.getSourceManager().getExpansionRange(loc);
    if (pair.first == pair.second){
      //okay, this is a macro token, not a macro expansion of the form macro(x,y)
      pair.second = clang::Lexer::getLocForEndOfToken(pair.second, 1,
                                       CI.getSourceManager(), CI.getLangOpts());
    }
  }

  FoundMacro(clang::CompilerInstance& c, clang::Stmt* s): CI(c), compound(false) {
    //need to figure out start and end of macro
    LocPair startExpansionRange; getLocations(s->getLocStart(), startExpansionRange);
    LocPair endExpansionRange; getLocations(s->getLocEnd(), endExpansionRange);
    start = startExpansionRange.first;
    end = startExpansionRange.second;
    if (endExpansionRange.second.isValid()){
      end = endExpansionRange.second;
    }
    //statement might cover multiple macros, unfortunately
    //unsigned MacroIDBit = 1U << 31; //because clang is a BoD with private functions
    //this is so dirty
    //unsigned maxID = s->getLocEnd().getRawEncoding() & ~MacroIDBit;
    //clang::SourceLocation maxEnd = clang::SourceLocation::getFromRawEncoding(maxID);
    //if (end < maxEnd) end = maxEnd;
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
    macros.emplace_back(*CI, s);
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
