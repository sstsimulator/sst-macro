#ifndef sstmac_bin_clang_PRINTALL_H
#define sstmac_bin_clang_PRINTALL_H

#include "clangHeaders.h"
#include <utility>
#include "util.h"

namespace pvt {

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

template <class Replacer>
struct PrintAll {

  void recurse(const Stmt* stmt, std::ostream& os, Replacer&& repl){
    #define recurse_case(type,stmt,os,...) \
      case(clang::Stmt::type##Class): { \
      auto ptr = clang::cast<type>(stmt); \
      if (repl.hasReplacement(ptr)){ \
        os << repl.getReplacement(ptr); \
      } else { \
        recurse##type(ptr,os,std::forward<Replacer>(repl)); \
      } } \
      break 

   switch(stmt->getStmtClass()){
    recurse_case(ForStmt,stmt,os,std::forward<Replacer>(repl));
    recurse_case(CompoundStmt,stmt,os,std::forward<Replacer>(repl));
    recurse_case(IfStmt,stmt,os,std::forward<Replacer>(repl));
    recurse_case(BinaryOperator,stmt,os,std::forward<Replacer>(repl));
    recurse_case(ParenExpr,stmt,os,std::forward<Replacer>(repl));
    recurse_case(CallExpr,stmt,os,std::forward<Replacer>(repl));
    recurse_case(CXXMemberCallExpr,stmt,os,std::forward<Replacer>(repl));
    recurse_case(MemberExpr,stmt,os,std::forward<Replacer>(repl));
    recurse_case(DeclRefExpr,stmt,os,std::forward<Replacer>(repl));
    recurse_case(ImplicitCastExpr,stmt,os,std::forward<Replacer>(repl));
    //recurse_case(CStyleCastExpr,stmt,os,std::forward<Replacer>(repl));
    //recurse_case(CXXFunctionalCastExpr,stmt,os,std::forward<Replacer>(repl));
    recurse_case(ArraySubscriptExpr,stmt,os,std::forward<Replacer>(repl));
    //recurse_case(DeclStmt,stmt,os,std::forward<Replacer>(repl));
    recurse_case(ReturnStmt,stmt,os,std::forward<Replacer>(repl));
    default: {
      PrettyPrinter pp;
      pp.print(stmt);
      os << pp.os.str();
      break;
    }
   }
#undef recurse_case
  }

  void recurseReturnStmt(const ReturnStmt* stmt, std::ostream& os, Replacer&& repl){
    os << "return ";
    recurse(stmt->getRetValue(), os, std::forward<Replacer>(repl));
  }

  void recurseForStmt(const ForStmt* stmt, std::ostream& os, Replacer&& repl){
    os << "for (";
    recurse(stmt->getInit(), os, std::forward<Replacer>(repl));
    os << "; ";
    recurse(stmt->getCond(), os, std::forward<Replacer>(repl));
    os << "; ";
    recurse(stmt->getInc(), os, std::forward<Replacer>(repl));
    os << "){\n";
    recurse(stmt->getBody(), os, std::forward<Replacer>(repl));
    os << "\n}";
  }

  void recurseCompoundStmt(const CompoundStmt* stmt, std::ostream& os, Replacer&& repl){
    auto end = stmt->child_end();
    for (auto iter=stmt->child_begin(); iter != end; ++iter){
      recurse(*iter, os, std::forward<Replacer>(repl));
    }
  }

  void recurseIfStmt(const IfStmt* stmt, std::ostream& os, Replacer&& repl){
    os << "if (";
    if (stmt->getInit()){
      recurse(stmt->getInit(), os, std::forward<Replacer>(repl));
      os << "=";
    }
    recurse(stmt->getCond(), os, std::forward<Replacer>(repl));
    os << "){";
    recurse(stmt->getThen(), os, std::forward<Replacer>(repl));
    os << "}";
    if (stmt->getElse()){
      os << " else {";
      recurse(stmt->getElse(), os, std::forward<Replacer>(repl));
      os << "}";
    }
  }

  void recurseBinaryOperator(const BinaryOperator* op, std::ostream& os, Replacer&& repl){
    recurse(op->getLHS(), os, std::forward<Replacer>(repl));
    os << op->getOpcodeStr().str();
    recurse(op->getRHS(), os, std::forward<Replacer>(repl));
  }

  void recurseParenExpr(const ParenExpr* expr, std::ostream& os, Replacer&& repl){
    //parentheticalness doesn't change the "os" of the variable
    os << "(";
    recurse(expr->getSubExpr(), os, std::forward<Replacer>(repl));
    os << ")";
  }

  void recurseCallExpr(const CallExpr* expr, std::ostream& os, Replacer&& repl){
    recurse(expr->getCallee(), os, std::forward<Replacer>(repl));
    os << "(";
    for (int i=0; i < expr->getNumArgs(); ++i){
      if (i > 0) os << ",";
      recurse(expr->getArg(i), os, std::forward<Replacer>(repl));
    }
    os << ")";
  }

  void recurseCXXMemberCallExpr(const CXXMemberCallExpr* expr, std::ostream& os, Replacer&& repl){
    //recurse(expr->getImplicitObjectArgument(), std::ostream&::ThisPtr, std::forward<Replacer>(args));
    recurseCallExpr(expr,os,repl);
  }

  void recurseMemberExpr(const MemberExpr* expr, std::ostream& os, Replacer&& repl){
    recurse(expr->getBase(), os, std::forward<Replacer>(repl));
    if (expr->isArrow()){
      os << "->";
    } else {
      os << ".";
    }
    os << expr->getMemberDecl()->getNameAsString();
  }

  void recurseDeclRefExpr(const DeclRefExpr* expr, std::ostream& os, Replacer&& repl){
    os << expr->getFoundDecl()->getNameAsString();
  }

  void recurseImplicitCastExpr(const ImplicitCastExpr* expr, std::ostream& os, Replacer&& repl){
    //implicit cast doesn't change the variable, carry on through
    recurse(expr->getSubExpr(), os, std::forward<Replacer>(repl));
  }

  void recurseArraySubscriptExpr(const ArraySubscriptExpr* expr, std::ostream& os, Replacer&& repl){
    recurse(expr->getBase(), os, std::forward<Replacer>(repl));
    os << "[";
    recurse(expr->getIdx(), os, std::forward<Replacer>(repl));
    os << "]";
  }
};

}

struct NullReplacement
{
  template <class T>
  bool hasReplacement(T* t){ return false; }

  template <class T>
  std::string getReplacement(T* t){ return ""; }
};

#include <sstream>

template <class Replacer>
void printAll(clang::Stmt* stmt, std::ostream& os, Replacer&& repl){
  pvt::PrintAll<Replacer>().recurse(stmt, os, std::forward<Replacer>(repl));
}

#endif // RECURSEALL_H
