#include "astVisitor.h"

using namespace clang;

static const Type*
getBaseType(VarDecl* D){
  auto ty = D->getType().getTypePtr();
  while (ty->isPointerType() && !(ty->isFunctionPointerType() || ty->isFunctionProtoType())){
    ty = ty->getPointeeType().getTypePtr();
  }
  return ty;
}

static std::string
getFxnTypedef(const Type* ty, const std::string& name){
  std::string typeName = GetAsString(ty);
  //this is horrible... but the only way I know
  auto pos = typeName.find(")");
  std::string tdefName = "typedef " + typeName.substr(0, pos) + name +typeName.substr(pos);
  return tdefName;
}

static void
scopeString(SourceLocation loc, CompilerInstance& ci, DeclContext* ctx,
            std::list<std::string>& scopes){
  while (ctx->getDeclKind() != Decl::TranslationUnit){
    switch (ctx->getDeclKind()){
      case Decl::Namespace: {
        NamespaceDecl* nsDecl = cast<NamespaceDecl>(ctx);
        ctx = nsDecl->getDeclContext();
        scopes.push_front(nsDecl->getNameAsString());
      }
      break;
      case Decl::CXXRecord: {
        CXXRecordDecl* clsDecl = cast<CXXRecordDecl>(ctx);
        ctx = clsDecl->getDeclContext();
        scopes.push_front(clsDecl->getNameAsString());
      }
      break;
    default:
      errorAbort(loc, ci, "bad context type in scope string");
      break;
    }
  }
}

SkeletonASTVisitor::GlobalVariableReplacement
SkeletonASTVisitor::setupGlobalReplacement(VarDecl *D, const std::string& namePrefix,
                                           bool useAccessor, bool isFxnStatic, bool needFullNs)
{
  bool threadLocal = isThreadLocal(D);
  if (threadLocal){
    errorAbort(D, *ci_,
               "thread local variables not yet allowed");
  }

  std::string uniqueName = namePrefix + D->getNameAsString();

  //if a pointer type, the subtype of the pointer
  const Type* ptrSubTy = nullptr;
  if (D->getType()->isPointerType()){
    ptrSubTy = getBaseType(D);
  }

  GlobalVariableReplacement var(uniqueName, useAccessor, isFxnStatic, needFullNs, threadLocal);
  var.anonRecord = checkAnonStruct(D);
  var.arrayInfo = checkArray(D);
  if (var.arrayInfo) {
    var.retType = var.arrayInfo->retType;
    var.typeStr = var.arrayInfo->typedefName;
    delayedInsertAfter(D, var.arrayInfo->typedefDeclString + ";");
  } else if (var.anonRecord){
    var.retType = var.anonRecord->retType;
    SourceLocation openBrace = var.anonRecord->decl->getBraceRange().getBegin();
    rewriter_.InsertText(openBrace, "  " + var.anonRecord->typeName, false, false);
    var.typeStr = var.anonRecord->typeName;
  } else if (D->getType()->isBooleanType()) {
    var.retType = "bool*";
    var.typeStr = "bool";
  } else if (isCxx() && D->getType()->isStructureOrClassType()) {
    //it's effing obnoxious that I will have to fix this at some point
    //the problem is that Clang in its stupidity doesn't print this type with correct namespacing
    //this produces obnoxious incomplete type or unknown type errors later
    //rather than directly calling D->getType()....
    //this is going to break at some point
    var.typeStr = getCleanName(GetAsString(D->getType()));
    var.retType = var.typeStr + "*";
  } else if (D->getType().getTypePtr()->isFunctionPointerType()){
    bool isTypeDef = isa<TypedefType>(D->getType().getTypePtr());
    if (isTypeDef){
      var.typeStr = GetAsString(D->getType());
      var.retType = var.typeStr + "*";
    } else {
      std::string typedefName = D->getNameAsString() + "_sstmac_fxn_typedef";
      std::string typedefDecl = getFxnTypedef(D->getType().getTypePtr(), typedefName);
      delayedInsertAfter(D, typedefDecl + ";");
      var.typeStr = typedefName;
      var.retType = typedefName + "*";
    }
  } else if (ptrSubTy && (ptrSubTy->isFunctionPointerType() || ptrSubTy->isFunctionProtoType())){
    bool isTypeDef = isa<TypedefType>(ptrSubTy);
    std::string typeStr = GetAsString(D->getType());
    if (isTypeDef){
      var.typeStr = typeStr;
      var.retType = var.typeStr + "*";
    } else {
      std::string typedefName = D->getNameAsString() + "_sstmac_fxn_typedef";
      std::string typedefDecl = getFxnTypedef(ptrSubTy, typedefName);
      int ptrDepth = std::count(typeStr.begin(), typeStr.end(), '*') - 1;
      var.typeStr = typedefName;
      for (int i=0; i < ptrDepth; i++){
        var.typeStr += "*";
      }
      var.retType = var.typeStr + "*";
      delayedInsertAfter(D, typedefDecl + ";");
    }
  } else {
    var.typeStr = GetAsString(D->getType());
    var.retType = var.typeStr + "*";
  }
  return var;
}

void
SkeletonASTVisitor::registerGlobalReplacement(VarDecl* D, GlobalVariableReplacement* repl)
{
  //std::string scopeUniqueVarName = scopePrefix + D->getNameAsString();
  setActiveGlobalScopedName(repl->scopeUniqueVarName);
  scopedNames_[mainDecl(D)] = repl->scopeUniqueVarName;

  if (!D->hasExternalStorage()){
    currentNs_->replVars.emplace(std::piecewise_construct,
          std::forward_as_tuple(repl->scopeUniqueVarName),
          std::forward_as_tuple(isCxx(), repl->isFxnStatic, repl->threadLocal));
  }

  SourceLocation declEnd = getEndLoc(getEnd(D));
  if (declEnd.isInvalid()){
    D->dump();
    errorAbort(D, *ci_,
               "unable to locate end of variable declaration");
  }

  const Decl* md = mainDecl(D);
  if (repl->useAccessor){
    globals_.insert({md, GlobalReplacement("", "_getter()", true)});
  } else {
    std::string standinName = "sstmac_" + repl->scopeUniqueVarName;
    if (repl->needFullNamespace){
      //we could have two variables named x in different namespaces
      //this just makes sure their global var replacements are called different things
      std::string uniqueNsPrefix = currentNs_->nsPrefix();
      for (int i=0; i < uniqueNsPrefix.size(); ++i){
        char& next = uniqueNsPrefix.at(i);
        if (next == ':') next = '_';
      }
      standinName = uniqueNsPrefix + standinName;
    }

    /**
     * we create two ways to use the global variable replacement
     * assume we have a global int a used in the expression
     * int sum = a + 3
     * at the beginning of the function we can create
     * void* sstmac_global_data = get_sstmac_global_data();
     * int* sstmac_a = (int*)(sstmac_global_data + __offset_a);
     * then we can replace every use of a with (*sstmac_a), e.g.
     * int sum = (*sstmac_a) + 3;
     * if a is used many times in a function, we essentially "cache" the replacement
     * every use of 'a' is replaced with 'sstmac_a'
     *
     * in some scenarios, it's not possible to create the variable sstmac_a
     * and we need to use the global variable directly inline
     * now we have to replace every use of 'a' with
     * *((int*)(get_sstmac_global_data() + __offset_a))
     *
     */

    std::stringstream cachedUseSstr;
    std::stringstream inlineUseSstr;
    cachedUseSstr << repl->retType << " " << standinName;
    inlineUseSstr << "(*((" << repl->retType << ")(";

    cachedUseSstr << "=(" << repl->retType << ")";
    if (repl->threadLocal){
      cachedUseSstr << "(sstmac_tls_data + ";
      inlineUseSstr << "get_sstmac_tls_data() + ";
    } else {
      cachedUseSstr << "(sstmac_global_data + ";
      inlineUseSstr << "get_sstmac_global_data() + ";
    }

    if (repl->needFullNamespace){
      cachedUseSstr << currentNs_->nsPrefix();
      inlineUseSstr << currentNs_->nsPrefix();
    }
    cachedUseSstr << repl->classScope;
    inlineUseSstr << repl->classScope;

    cachedUseSstr << "__offset_" << repl->scopeUniqueVarName << ")";
    inlineUseSstr << "__offset_" << repl->scopeUniqueVarName << ")))";

    GlobalStandin& newStandin = globalStandins_[md];
    newStandin.replText = cachedUseSstr.str();
    newStandin.threadLocal = repl->threadLocal;

    std::stringstream replSstr;
    replSstr << "(*" << standinName << ")";
    globals_.insert({md, GlobalReplacement(replSstr.str(), inlineUseSstr.str(), false)});
  }
}

bool
SkeletonASTVisitor::setupClassStaticVarDecl(VarDecl* D)
{
  SourceLocation declEnd = getEndLoc(getEnd(D));
  bool threadLocal = isThreadLocal(D);
  //later on we will have to actually handle its definition in another file
  if (D->getDeclContext()->isDependentContext()){
    dependentStaticMembers_[D->getNameAsString()] = D;
    //don't privatize this anymore - it breaks all sorts of things like decltype
    //we used to privatize variables to force compiler errors

    //create an accessor method for the variable
    //use decltype for return... this can create weirdness

    GlobalVariableReplacement repl = setupGlobalReplacement(D, "",
                              false, /*uses of the variable don't need any extra scope prefixes*/
                              true, /*this should be accessed through the special getter method above*/
                              false /*don't need full ns added*/);
    std::stringstream os;
    os << "static " << repl.typeStr << "& "
        << D->getNameAsString() << "_getter(){ "
        << " char* data = " << (threadLocal ? "get_sstmac_tls_data(); " : "get_sstmac_global_data(); ")
        << " void* offsetPtr = data + __offset_" << D->getNameAsString() << ";"
        << "return *((" << repl.retType << ")(offsetPtr));"
        << "}";
    //everywhere the variable A->x or A.x is used will be replaced with the function above to be
    //A->x_getter() or A.x_getter()
    rewriter_.InsertText(declEnd, os.str());

    registerGlobalReplacement(D, &repl);
  } else {
    CXXRecordDecl* outerCls = classContexts_.front();
    std::stringstream varname_scope_sstr; varname_scope_sstr << "_";
    std::stringstream cls_scope_sstr;
    for (CXXRecordDecl* decl : classContexts_){
      varname_scope_sstr << "_" << decl->getNameAsString();
      cls_scope_sstr << decl->getNameAsString() << "::";
    }

    GlobalVariableReplacement repl = setupGlobalReplacement(D, "", false, false, true);
    repl.classScope = cls_scope_sstr.str();
    registerGlobalReplacement(D, &repl);
  }

  std::string offsetDecl = " public: static int __offset_" + D->getNameAsString() + ";";
  delayedInsertAfter(D, offsetDecl);
  return true;
}

bool
SkeletonASTVisitor::setupCGlobalVar(VarDecl* D, const std::string& scopePrefix)
{
  Expr* init = D->hasInit() ? getUnderlyingExpr(D->getInit()) : nullptr;

  std::stringstream newVarSstr;
  GlobalVariableReplacement var = setupGlobalReplacement(D, scopePrefix, false, false, true);
  newVarSstr << "extern int __offset_" << var.scopeUniqueVarName << "; ";
  if (!D->hasExternalStorage()){
    std::string initFxnName = "init_" + var.scopeUniqueVarName;
    //add an init function for it
    newVarSstr << "void " << initFxnName << "(void* ptr){";

    //hacky... but, oh well
    //auto constPos = var.typeStr.find("const ");
    //std::string tmpTypeStr = var.typeStr;
    //if (constPos != std::string::npos && constPos == 0){
    //  tmpTypeStr = tmpTypeStr.substr(constPos + 6);
    //}

    //recreate the global variable temporarily in this function,
    //but with globals refs correctly replaced
    if (init){
      newVarSstr << var.typeStr << " initer = "
          << printWithGlobalsReplaced(init) << "; ";
      newVarSstr << "memcpy(ptr, &initer, sizeof(initer));";
      //newVarSstr << tmpTypeStr << "* tmpPtr = " << "(" << tmpTypeStr << "*)ptr; "
      //            << "*tmpPtr = " << printWithGlobalsReplaced(init) << ";";
    }
    newVarSstr << " }";
    newVarSstr << "int __sizeof_" << var.scopeUniqueVarName << " = sizeof(" << var.typeStr << "); ";
  }
  registerGlobalReplacement(D, &var);
  delayedInsertAfter(D, newVarSstr.str());

  return true;
}

bool
SkeletonASTVisitor::setupCppGlobalVar(VarDecl* D, const std::string& scopePrefix)
{
  Expr* init = D->hasInit() ? getUnderlyingExpr(D->getInit()) : nullptr;
  Stmt* cppCtorArgs = nullptr;
  RecordDecl* rd = D->getType().getTypePtr()->getAsCXXRecordDecl();
  if (!D->hasExternalStorage()){
    if (rd){
      cppCtorArgs = getCtor(D);
    } else if (init) {
      cppCtorArgs = init;
    }
  }



  std::stringstream newVarSstr;
  GlobalVariableReplacement repl = setupGlobalReplacement(D, scopePrefix, false, false, true);
  newVarSstr << "extern int __offset_" << repl.scopeUniqueVarName << "; ";
  if (!D->hasExternalStorage()){
    newVarSstr << "int __sizeof_" << repl.scopeUniqueVarName << " = sizeof(" << repl.typeStr << ");";
    auto pos = repl.typeStr.find("struct ");
    if (pos != std::string::npos){
      repl.typeStr = eraseAllStructQualifiers(repl.typeStr);
    }

    if (D->getStorageClass() == SC_Static){
      newVarSstr << "static ";
    }
    newVarSstr << "sstmac::CppGlobalRegisterGuard "
               << D->getNameAsString() << "_sstmac_ctor("
               << "__offset_" << repl.scopeUniqueVarName
               << ", __sizeof_" << repl.scopeUniqueVarName
               << ", " << std::boolalpha << repl.threadLocal
               << ", \"" << D->getNameAsString() << "\""
               << ", [](void* ptr){ "
               << " new (ptr) " << repl.typeStr;

    if (cppCtorArgs){
      if (cppCtorArgs->getStmtClass() != Stmt::InitListExprClass){
        newVarSstr << "{";
      }
      newVarSstr << printWithGlobalsReplaced(cppCtorArgs);
      if (cppCtorArgs->getStmtClass() != Stmt::InitListExprClass){
        newVarSstr << "}";
      }
    }
    newVarSstr << "; });";
    //the original variable is gone... but for subtle reasons
    //we need the variable to be reference-able for decltype and sizeof operators later

    //this is a super annoying way to have to do this
    //if we have multi-declarations int x,y, e.g.
    //the var decls overlap and we need to avoid deleting the same code region twice
  }
  registerGlobalReplacement(D, &repl);
  delayedInsertAfter(D, newVarSstr.str());
  return true;
}

bool
SkeletonASTVisitor::setupFunctionStaticCpp(VarDecl* D, const std::string& scopePrefix)
{
  internalError(D, *ci_, "cannot currently handle static function variables in C++");
  return true;
  /**
  if (insideTemplateFxn()){
    //okay, this is a little bit weird
    //we have a template parameter
    needExternalDecls = false;
    checkCxxCtor = false;
    needClsScope = false;
    //the offset declaration actually goes here
    //if (innerStructTagsDeclared_.find(D->getDeclContext()) == innerStructTagsDeclared_.end()){
      //keep from declaring this more than once per decl context
    os << "struct inner_" << scopeUniqueVarName << "{};";
    //  innerStructTagsDeclared_.insert(D->getDeclContext());
    internalError(D, *ci_, "cannot currently handle static function variables in a template function");
    os << "static int __offset_" << scopeUniqueVarName
       << " = sstmac::inplaceCppGlobal<"
       << "inner_" << scopeUniqueVarName
       << "," << getCleanTypeName(D->getType())
       << "," << std::boolalpha << threadLocal
       << ">(";
    addInitializers(D, os, false); //no leading comma
    os << ");";
  }
  */
}

bool
SkeletonASTVisitor::setupFunctionStaticC(VarDecl* D, const std::string& scopePrefix)
{
  internalError(D, *ci_, "cannot currently handle static function variables in C");
  return true;
  /**
    if (global_var_ty == FxnStatic){
      newStandin.fxnStatic = true;
      //we need to put the global standin text here!
      std::string newText = newStandin.replText + "; ";
      rewriter_.InsertText(declEnd, newText);
      //it's possible this never gets referenced because of silly code
      //make sure this gets added to globals touched lists
      //regardless of whether it shows up in a DeclRefExpr
      globalsTouched_.back().insert(md);
    }

  os << "static int sstmac_inited_" << D->getNameAsString() << " = 0;"
    << "if (!sstmac_inited_" << D->getNameAsString() << "){"
    << "  sstmac_init_global_space(&" << D->getNameAsString()
       << "," << "__sizeof_" << scopeUniqueVarName
       << "," << "__offset_" << scopeUniqueVarName
       << "," << (threadLocal ? "1" : "0") //because of C, can't guarantee bools defined
       << ");"
    << "  sstmac_inited_" << D->getNameAsString() << " = 1; "
    << "}";
  std::stringstream size_os;
  std::map<const RecordDecl*, ReconstructedType> newTypes;
  std::string typeNameToSize = getTypeNameForSizing(getStart(fxnContexts_.front()),
                                                    D->getType(), newTypes);
  if (typeNameToSize.empty()){
    errorAbort(D, *ci_,
               "internal error: empty type name for variable");
  }
  std::set<const RecordDecl*> alreadyDone;
  for (auto& pair : newTypes){
    addTypeReconstructionText(pair.first, pair.second, newTypes, alreadyDone, size_os);
  }
  size_os << "int __sizeof_" << scopeUniqueVarName << " = "
          << "sizeof(" << typeNameToSize << "); ";
  rewriter_.InsertText(varSizeOfInsertLoc, size_os.str());
  */
}

bool
SkeletonASTVisitor::checkDeclStaticClassVar(VarDecl *D)
{
  if (classContexts_.size() > 1){
    errorAbort(D, *ci_, "cannot handle static variables in inner classes");
  }


  if (!D->hasInit()){
    setupClassStaticVarDecl(D);
  } //else this must be a const integer if inited in the header file
  //we don't have to "deglobalize" this

  return false;
}

bool
SkeletonASTVisitor::checkStaticFileVar(VarDecl* D)
{
  std::string prefix = currentNs_->filePrefix(ci_, getStart(D));
  if (isCxx()){
    return setupCppGlobalVar(D, prefix);
  } else {
    return setupCGlobalVar(D, prefix);
  }
}

bool
SkeletonASTVisitor::checkGlobalVar(VarDecl* D)
{
  if (isCxx()){
    return setupCppGlobalVar(D, "");
  } else {
    return setupCGlobalVar(D, "");
  }
}

bool
SkeletonASTVisitor::checkStaticFxnVar(VarDecl *D)
{
  FunctionDecl* outerFxn = fxnContexts_.front();
  std::stringstream prefix_sstr;
  prefix_sstr << "_" << outerFxn->getNameAsString();

  auto& staticVars = staticFxnVarCounts_[outerFxn];
  int& cnt = staticVars[D->getNameAsString()];
  //due to scoping, we might have several static fxn vars
  //all with the same name
  if (cnt != 0){
    prefix_sstr << "_" << cnt;
  }
  ++cnt;

  std::string scope_prefix = currentNs_->filePrefix(ci_, getStart(D)) + prefix_sstr.str();

  if (isCxx()){
    return setupFunctionStaticCpp(D, scope_prefix);
  } else {
    return setupFunctionStaticC(D, scope_prefix);
  }
}

bool
SkeletonASTVisitor::checkInstanceStaticClassVar(VarDecl *D)
{
  auto iter = globals_.find(mainDecl(D));
  if (iter == globals_.end()){
    //hmm, this must be const or something
    //because the main decl didn't end up in the globals
    return true;
  }

  //okay - this sucks - I have no idea how this variable is being declared
  //it could be ns::A::x = 10 for a variable in namespace ns, class A
  //it could namespace ns { A::x = 10 }
  //we must have the variable declarations in the full namespace - we can't cheat
  DeclContext* lexicalContext = D->getLexicalDeclContext();
  DeclContext* semanticContext = D->getDeclContext();
  if (!isa<CXXRecordDecl>(semanticContext)){
    std::string error = "variable " + D->getNameAsString() + " does not have class semantic context";
    errorAbort(D, *ci_, error);
  }
  CXXRecordDecl* parentCls = cast<CXXRecordDecl>(semanticContext);
  std::list<std::string> lex;
  scopeString(getStart(D), *ci_, lexicalContext, lex);
  std::list<std::string> sem;
  scopeString(getStart(D), *ci_, semanticContext, sem);

  //match the format from checkDeclStaticClassVar

  std::stringstream os;
  //throw away the class name in the list of scopes
  sem.pop_back();
  auto semBegin = sem.begin();
  for (auto& ignore : lex){ //figure out the overlap between lexical and semantic namespaces
    ++semBegin;
  }
  for (auto iter = semBegin; iter != sem.end(); ++iter){ //I must init/declare vars in the most enclosing namespace
    os << "namespace " << *iter << " {";
  }

  std::string clsName = parentCls->getNameAsString();
  std::string tagName = "UniqueTag" + clsName + D->getNameAsString();
  os << "struct " << tagName << "{}; ";
  if (parentCls->isDependentContext()){
    std::stringstream tmplSstr; //the name A<T,U>
    tmplSstr << clsName << "<";
    int numLists = D->getNumTemplateParameterLists();
    if (numLists > 1){
      internalError(getStart(D), *ci_,
          "cannot handle nested template declarations");
    }
    TemplateParameterList* theList = D->getTemplateParameterList(0);
    getTemplatePrefixString(os, theList);
    getTemplateParamsString(tmplSstr, theList);
    clsName = tmplSstr.str();
  }

  GlobalVariableReplacement repl = setupGlobalReplacement(D, "", false, false, true);
  os << "int " << clsName
     << "::__offset_" << D->getNameAsString()
     << " = sstmac::inplaceCppGlobal<"
     << tagName
     << "," << repl.typeStr
     << "," << std::boolalpha << isThreadLocal(D)
     << ">(" << "\"" << D->getNameAsString() << "\""
     << ",[](void* ptr){ "
     << "    new (ptr) " << repl.typeStr;

  if (D->hasInit()){
    if (D->getInit()->getStmtClass() != Stmt::InitListExprClass){
      os << "{";
    }
    os << printWithGlobalsReplaced(D->getInit());
    if (D->getInit()->getStmtClass() != Stmt::InitListExprClass){
      os << "}";
    }
  } else {
    os << "{}; ";
  }
  os << "; });";

  for (auto iter = semBegin; iter != sem.end(); ++iter){
    os << "}"; //close namespaces
  }

  delayedInsertAfter(D, os.str());

  return true;
}
