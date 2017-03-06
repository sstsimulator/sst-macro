#include "replAstVisitor.h"
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

bool
ReplGlobalASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr){
  NamedDecl* decl =  expr->getFoundDecl();
  ValueDecl* val = expr->getDecl();
#if 0
  std::cout << "Visting use of variable  " << decl->getName().str()
            << " " << val->getName().str()
            << "\n " << val->getLocStart().printToString(CI.getSourceManager())
            << "\n " << val->getLocEnd().printToString(CI.getSourceManager())
            << "\n " << expr->getLocStart().printToString(CI.getSourceManager())
            << "\n " << expr->getLocEnd().printToString(CI.getSourceManager())
            << std::endl;
  SourceLocation startLoc = expr->getLocStart();
  SourceLocation endLoc = expr->getLocEnd();
  if( startLoc.isMacroID() ) {
      // Get the start/end expansion locations
      std::pair< SourceLocation, SourceLocation > expansionRange =
               CI.getSourceManager().getExpansionRange( startLoc );

      // We're just interested in the start location
      SourceLocation startLoc = expansionRange.first;
      SourceLocation endLoc = expansionRange.second;
      std::cout << "Variable " << decl->getName().str() << " found from"
             << "\n  " << startLoc.printToString(CI.getSourceManager())
             << "\n  " << endLoc.printToString(CI.getSourceManager())
             << std::endl;
      std::cout << std::boolalpha << (startLoc == endLoc) << std::endl;
  }

  if( startLoc.isMacroID() ) {
      std::pair< SourceLocation, SourceLocation > expansionRange =
            CI.getSourceManager().getImmediateExpansionRange( startLoc );
      SourceLocation startLoc = expansionRange.first;
      SourceLocation endLoc = expansionRange.second;
      std::cout << "Variable " << decl->getName().str() << " found from"
             << "\n  " << startLoc.printToString(CI.getSourceManager())
             << "\n  " << endLoc.printToString(CI.getSourceManager())
             << std::endl;

      endLoc = Lexer::getLocForEndOfToken(endLoc, 0, CI.getSourceManager(), CI.getLangOpts());
      std::cout << "Variable " << decl->getName().str() << " found from"
             << "\n  " << startLoc.printToString(CI.getSourceManager())
             << "\n  " << endLoc.printToString(CI.getSourceManager())
             << std::endl;
  }
#endif

  auto globals = finder.globalVariables();
  auto iter = globals.find(decl);
  if (iter != globals.end()){
    TheRewriter.ReplaceText(expr->getSourceRange(), iter->second);
  }

  return true;
}


