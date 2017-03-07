#include "util.h"
#include <list>

struct CodeItem {
  CodeItem(clang::Stmt* s) : stmt(s), decl(nullptr){}
  CodeItem(clang::Decl* d) : decl(d), stmt(nullptr){}

  clang::Stmt* stmt;
  clang::Decl* decl;
};


struct FoundMacro {
  std::list<CodeItem> items;
  clang::SourceLocation start;
  clang::SourceLocation end;

  FoundMacro(clang::CompilerInstance& CI, clang::SourceLocation loc){
    //need to figure out start and end of macro
    auto expansionRange = CI.getSourceManager().getExpansionRange(loc);
    if (expansionRange.first == expansionRange.second){
      //okay, this is a macro token, not a macro expansion of the form macro(x,y)
      expansionRange.second = clang::Lexer::getLocForEndOfToken(expansionRange.second, 0,
                                       CI.getSourceManager(), CI.getLangOpts());
    }
    start = expansionRange.first;
    end = expansionRange.second;
  }

  template <class T>
  bool overlaps(T* t){
    clang::SourceLocation Tst = t->getLocStart();
    clang::SourceLocation Tend = t->getLocEnd();
    return start <= Tst && Tend <= end;
  }

  template <class T>
  void append(T* t){
    items.emplace_back(t);
  }

  template <class T>
  bool appendIfOverlap(T* t){
    bool over = overlaps<T>(t);
    if (over){
      items.emplace_back(t);
    }
    return over;
  }
};

struct MacroList {
  MacroList(clang::CompilerInstance& c) :
    current(nullptr), CI(c) {}
  FoundMacro* current;
  std::list<FoundMacro> macros;
  clang::CompilerInstance& CI;

  template <class T>
  void appendNew(T* t){
    macros.emplace_back(CI, t->getLocStart());
    macros.back().append(t);
  }

  /**
   * Should only be invoked when the stmt/decl is definitely within a macro
   */
  template <class T>
  void append(T* t){
    if (macros.empty()) appendNew(t);
    bool matches = macros.back().appendIfOverlap(t);
    if (!matches){
      appendNew(t);
    }
  }
};
