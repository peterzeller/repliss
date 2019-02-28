/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class Parser {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected Parser(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(Parser obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_Parser(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public ExprManager getExprManager() {
    long cPtr = CVC4JNI.Parser_getExprManager(swigCPtr, this);
    return (cPtr == 0) ? null : new ExprManager(cPtr, false);
  }

  public Input getInput() {
    long cPtr = CVC4JNI.Parser_getInput(swigCPtr, this);
    return (cPtr == 0) ? null : new Input(cPtr, false);
  }

  public void setInput(Input input) {
    CVC4JNI.Parser_setInput(swigCPtr, this, Input.getCPtr(input), input);
  }

  public boolean done() {
    return CVC4JNI.Parser_done(swigCPtr, this);
  }

  public void setDone(boolean done) {
    CVC4JNI.Parser_setDone__SWIG_0(swigCPtr, this, done);
  }

  public void setDone() {
    CVC4JNI.Parser_setDone__SWIG_1(swigCPtr, this);
  }

  public void enableChecks() {
    CVC4JNI.Parser_enableChecks(swigCPtr, this);
  }

  public void disableChecks() {
    CVC4JNI.Parser_disableChecks(swigCPtr, this);
  }

  public void enableStrictMode() {
    CVC4JNI.Parser_enableStrictMode(swigCPtr, this);
  }

  public void disableStrictMode() {
    CVC4JNI.Parser_disableStrictMode(swigCPtr, this);
  }

  public boolean strictModeEnabled() {
    return CVC4JNI.Parser_strictModeEnabled(swigCPtr, this);
  }

  public void allowIncludeFile() {
    CVC4JNI.Parser_allowIncludeFile(swigCPtr, this);
  }

  public void disallowIncludeFile() {
    CVC4JNI.Parser_disallowIncludeFile(swigCPtr, this);
  }

  public boolean canIncludeFile() {
    return CVC4JNI.Parser_canIncludeFile(swigCPtr, this);
  }

  public boolean logicIsSet() {
    return CVC4JNI.Parser_logicIsSet(swigCPtr, this);
  }

  public void forceLogic(String logic) {
    CVC4JNI.Parser_forceLogic(swigCPtr, this, logic);
  }

  public String getForcedLogic() {
    return CVC4JNI.Parser_getForcedLogic(swigCPtr, this);
  }

  public boolean logicIsForced() {
    return CVC4JNI.Parser_logicIsForced(swigCPtr, this);
  }

  public Expr getVariable(String name) {
    return new Expr(CVC4JNI.Parser_getVariable(swigCPtr, this, name), true);
  }

  public Expr getFunction(String name) {
    return new Expr(CVC4JNI.Parser_getFunction(swigCPtr, this, name), true);
  }

  public Expr getExpressionForName(String name) {
    return new Expr(CVC4JNI.Parser_getExpressionForName(swigCPtr, this, name), true);
  }

  public Expr getExpressionForNameAndType(String name, Type t) {
    return new Expr(CVC4JNI.Parser_getExpressionForNameAndType(swigCPtr, this, name, Type.getCPtr(t), t), true);
  }

  public Kind getKindForFunction(Expr fun) {
    return Kind.swigToEnum(CVC4JNI.Parser_getKindForFunction(swigCPtr, this, Expr.getCPtr(fun), fun));
  }

  public Type getSort(String sort_name) {
    return new Type(CVC4JNI.Parser_getSort__SWIG_0(swigCPtr, this, sort_name), true);
  }

  public Type getSort(String sort_name, vectorType params) {
    return new Type(CVC4JNI.Parser_getSort__SWIG_1(swigCPtr, this, sort_name, vectorType.getCPtr(params), params), true);
  }

  public long getArity(String sort_name) {
    return CVC4JNI.Parser_getArity(swigCPtr, this, sort_name);
  }

  public boolean isDeclared(String name, SymbolType type) {
    return CVC4JNI.Parser_isDeclared__SWIG_0(swigCPtr, this, name, type.swigValue());
  }

  public boolean isDeclared(String name) {
    return CVC4JNI.Parser_isDeclared__SWIG_1(swigCPtr, this, name);
  }

  public void checkDeclaration(String name, DeclarationCheck check, SymbolType type, String notes) {
    CVC4JNI.Parser_checkDeclaration__SWIG_0(swigCPtr, this, name, check.swigValue(), type.swigValue(), notes);
  }

  public void checkDeclaration(String name, DeclarationCheck check, SymbolType type) {
    CVC4JNI.Parser_checkDeclaration__SWIG_1(swigCPtr, this, name, check.swigValue(), type.swigValue());
  }

  public void checkDeclaration(String name, DeclarationCheck check) {
    CVC4JNI.Parser_checkDeclaration__SWIG_2(swigCPtr, this, name, check.swigValue());
  }

  public void reserveSymbolAtAssertionLevel(String name) {
    CVC4JNI.Parser_reserveSymbolAtAssertionLevel(swigCPtr, this, name);
  }

  public void checkFunctionLike(Expr fun) {
    CVC4JNI.Parser_checkFunctionLike(swigCPtr, this, Expr.getCPtr(fun), fun);
  }

  public void checkArity(Kind kind, long numArgs) {
    CVC4JNI.Parser_checkArity(swigCPtr, this, kind.swigValue(), numArgs);
  }

  public void checkOperator(Kind kind, long numArgs) {
    CVC4JNI.Parser_checkOperator(swigCPtr, this, kind.swigValue(), numArgs);
  }

  public Expr mkVar(String name, Type type, long flags, boolean doOverload) {
    return new Expr(CVC4JNI.Parser_mkVar__SWIG_0(swigCPtr, this, name, Type.getCPtr(type), type, flags, doOverload), true);
  }

  public Expr mkVar(String name, Type type, long flags) {
    return new Expr(CVC4JNI.Parser_mkVar__SWIG_1(swigCPtr, this, name, Type.getCPtr(type), type, flags), true);
  }

  public Expr mkVar(String name, Type type) {
    return new Expr(CVC4JNI.Parser_mkVar__SWIG_2(swigCPtr, this, name, Type.getCPtr(type), type), true);
  }

  public vectorExpr mkVars(vectorString names, Type type, long flags, boolean doOverload) {
    return new vectorExpr(CVC4JNI.Parser_mkVars__SWIG_0(swigCPtr, this, vectorString.getCPtr(names), names, Type.getCPtr(type), type, flags, doOverload), true);
  }

  public vectorExpr mkVars(vectorString names, Type type, long flags) {
    return new vectorExpr(CVC4JNI.Parser_mkVars__SWIG_1(swigCPtr, this, vectorString.getCPtr(names), names, Type.getCPtr(type), type, flags), true);
  }

  public vectorExpr mkVars(vectorString names, Type type) {
    return new vectorExpr(CVC4JNI.Parser_mkVars__SWIG_2(swigCPtr, this, vectorString.getCPtr(names), names, Type.getCPtr(type), type), true);
  }

  public Expr mkBoundVar(String name, Type type) {
    return new Expr(CVC4JNI.Parser_mkBoundVar(swigCPtr, this, name, Type.getCPtr(type), type), true);
  }

  public vectorExpr mkBoundVars(vectorString names, Type type) {
    return new vectorExpr(CVC4JNI.Parser_mkBoundVars(swigCPtr, this, vectorString.getCPtr(names), names, Type.getCPtr(type), type), true);
  }

  public Expr mkFunction(String name, Type type, long flags, boolean doOverload) {
    return new Expr(CVC4JNI.Parser_mkFunction__SWIG_0(swigCPtr, this, name, Type.getCPtr(type), type, flags, doOverload), true);
  }

  public Expr mkFunction(String name, Type type, long flags) {
    return new Expr(CVC4JNI.Parser_mkFunction__SWIG_1(swigCPtr, this, name, Type.getCPtr(type), type, flags), true);
  }

  public Expr mkFunction(String name, Type type) {
    return new Expr(CVC4JNI.Parser_mkFunction__SWIG_2(swigCPtr, this, name, Type.getCPtr(type), type), true);
  }

  public Expr mkAnonymousFunction(String prefix, Type type, long flags) {
    return new Expr(CVC4JNI.Parser_mkAnonymousFunction__SWIG_0(swigCPtr, this, prefix, Type.getCPtr(type), type, flags), true);
  }

  public Expr mkAnonymousFunction(String prefix, Type type) {
    return new Expr(CVC4JNI.Parser_mkAnonymousFunction__SWIG_1(swigCPtr, this, prefix, Type.getCPtr(type), type), true);
  }

  public void defineVar(String name, Expr val, boolean levelZero, boolean doOverload) {
    CVC4JNI.Parser_defineVar__SWIG_0(swigCPtr, this, name, Expr.getCPtr(val), val, levelZero, doOverload);
  }

  public void defineVar(String name, Expr val, boolean levelZero) {
    CVC4JNI.Parser_defineVar__SWIG_1(swigCPtr, this, name, Expr.getCPtr(val), val, levelZero);
  }

  public void defineVar(String name, Expr val) {
    CVC4JNI.Parser_defineVar__SWIG_2(swigCPtr, this, name, Expr.getCPtr(val), val);
  }

  public void defineFunction(String name, Expr val, boolean levelZero, boolean doOverload) {
    CVC4JNI.Parser_defineFunction__SWIG_0(swigCPtr, this, name, Expr.getCPtr(val), val, levelZero, doOverload);
  }

  public void defineFunction(String name, Expr val, boolean levelZero) {
    CVC4JNI.Parser_defineFunction__SWIG_1(swigCPtr, this, name, Expr.getCPtr(val), val, levelZero);
  }

  public void defineFunction(String name, Expr val) {
    CVC4JNI.Parser_defineFunction__SWIG_2(swigCPtr, this, name, Expr.getCPtr(val), val);
  }

  public void defineType(String name, Type type) {
    CVC4JNI.Parser_defineType__SWIG_0(swigCPtr, this, name, Type.getCPtr(type), type);
  }

  public void defineType(String name, vectorType params, Type type) {
    CVC4JNI.Parser_defineType__SWIG_1(swigCPtr, this, name, vectorType.getCPtr(params), params, Type.getCPtr(type), type);
  }

  public void defineParameterizedType(String name, vectorType params, Type type) {
    CVC4JNI.Parser_defineParameterizedType(swigCPtr, this, name, vectorType.getCPtr(params), params, Type.getCPtr(type), type);
  }

  public SortType mkSort(String name, long flags) {
    return new SortType(CVC4JNI.Parser_mkSort__SWIG_0(swigCPtr, this, name, flags), true);
  }

  public SortType mkSort(String name) {
    return new SortType(CVC4JNI.Parser_mkSort__SWIG_1(swigCPtr, this, name), true);
  }

  public SortConstructorType mkSortConstructor(String name, long arity) {
    return new SortConstructorType(CVC4JNI.Parser_mkSortConstructor(swigCPtr, this, name, arity), true);
  }

  public SortType mkUnresolvedType(String name) {
    return new SortType(CVC4JNI.Parser_mkUnresolvedType(swigCPtr, this, name), true);
  }

  public SortConstructorType mkUnresolvedTypeConstructor(String name, long arity) {
    return new SortConstructorType(CVC4JNI.Parser_mkUnresolvedTypeConstructor__SWIG_0(swigCPtr, this, name, arity), true);
  }

  public SortConstructorType mkUnresolvedTypeConstructor(String name, vectorType params) {
    return new SortConstructorType(CVC4JNI.Parser_mkUnresolvedTypeConstructor__SWIG_1(swigCPtr, this, name, vectorType.getCPtr(params), params), true);
  }

  public boolean isUnresolvedType(String name) {
    return CVC4JNI.Parser_isUnresolvedType(swigCPtr, this, name);
  }

  public vectorDatatypeType mkMutualDatatypeTypes(vectorDatatype datatypes, boolean doOverload) {
    return new vectorDatatypeType(CVC4JNI.Parser_mkMutualDatatypeTypes__SWIG_0(swigCPtr, this, vectorDatatype.getCPtr(datatypes), datatypes, doOverload), true);
  }

  public vectorDatatypeType mkMutualDatatypeTypes(vectorDatatype datatypes) {
    return new vectorDatatypeType(CVC4JNI.Parser_mkMutualDatatypeTypes__SWIG_1(swigCPtr, this, vectorDatatype.getCPtr(datatypes), datatypes), true);
  }

  public Type mkFlatFunctionType(vectorType sorts, Type range, vectorExpr flattenVars) {
    return new Type(CVC4JNI.Parser_mkFlatFunctionType__SWIG_0(swigCPtr, this, vectorType.getCPtr(sorts), sorts, Type.getCPtr(range), range, vectorExpr.getCPtr(flattenVars), flattenVars), true);
  }

  public Type mkFlatFunctionType(vectorType sorts, Type range) {
    return new Type(CVC4JNI.Parser_mkFlatFunctionType__SWIG_1(swigCPtr, this, vectorType.getCPtr(sorts), sorts, Type.getCPtr(range), range), true);
  }

  public Expr mkHoApply(Expr expr, vectorExpr args, long startIndex) {
    return new Expr(CVC4JNI.Parser_mkHoApply__SWIG_0(swigCPtr, this, Expr.getCPtr(expr), expr, vectorExpr.getCPtr(args), args, startIndex), true);
  }

  public Expr mkHoApply(Expr expr, vectorExpr args) {
    return new Expr(CVC4JNI.Parser_mkHoApply__SWIG_1(swigCPtr, this, Expr.getCPtr(expr), expr, vectorExpr.getCPtr(args), args), true);
  }

  public void addOperator(Kind kind) {
    CVC4JNI.Parser_addOperator(swigCPtr, this, kind.swigValue());
  }

  public void preemptCommand(Command cmd) {
    CVC4JNI.Parser_preemptCommand(swigCPtr, this, Command.getCPtr(cmd), cmd);
  }

  public boolean isBoolean(String name) {
    return CVC4JNI.Parser_isBoolean(swigCPtr, this, name);
  }

  public boolean isFunctionLike(Expr fun) {
    return CVC4JNI.Parser_isFunctionLike(swigCPtr, this, Expr.getCPtr(fun), fun);
  }

  public boolean isDefinedFunction(String name) {
    return CVC4JNI.Parser_isDefinedFunction__SWIG_0(swigCPtr, this, name);
  }

  public boolean isDefinedFunction(Expr func) {
    return CVC4JNI.Parser_isDefinedFunction__SWIG_1(swigCPtr, this, Expr.getCPtr(func), func);
  }

  public boolean isPredicate(String name) {
    return CVC4JNI.Parser_isPredicate(swigCPtr, this, name);
  }

  public Command nextCommand() {
    long cPtr = CVC4JNI.Parser_nextCommand(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Expr nextExpression() {
    return new Expr(CVC4JNI.Parser_nextExpression(swigCPtr, this), true);
  }

  public void warning(String msg) {
    CVC4JNI.Parser_warning(swigCPtr, this, msg);
  }

  public void attributeNotSupported(String attr) {
    CVC4JNI.Parser_attributeNotSupported(swigCPtr, this, attr);
  }

  public void parseError(String msg) {
    CVC4JNI.Parser_parseError(swigCPtr, this, msg);
  }

  public void unexpectedEOF(String msg) {
    CVC4JNI.Parser_unexpectedEOF(swigCPtr, this, msg);
  }

  public void unimplementedFeature(String msg) {
    CVC4JNI.Parser_unimplementedFeature(swigCPtr, this, msg);
  }

  public long scopeLevel() {
    return CVC4JNI.Parser_scopeLevel(swigCPtr, this);
  }

  public void pushScope(boolean bindingLevel) {
    CVC4JNI.Parser_pushScope__SWIG_0(swigCPtr, this, bindingLevel);
  }

  public void pushScope() {
    CVC4JNI.Parser_pushScope__SWIG_1(swigCPtr, this);
  }

  public void popScope() {
    CVC4JNI.Parser_popScope(swigCPtr, this);
  }

  public void reset() {
    CVC4JNI.Parser_reset(swigCPtr, this);
  }

  public void setGlobalDeclarations(boolean flag) {
    CVC4JNI.Parser_setGlobalDeclarations(swigCPtr, this, flag);
  }

  public void useDeclarationsFrom(Parser parser) {
    CVC4JNI.Parser_useDeclarationsFrom__SWIG_0(swigCPtr, this, Parser.getCPtr(parser), parser);
  }

  public void useDeclarationsFrom(SymbolTable symtab) {
    CVC4JNI.Parser_useDeclarationsFrom__SWIG_1(swigCPtr, this, SymbolTable.getCPtr(symtab), symtab);
  }

  public SymbolTable getSymbolTable() {
    long cPtr = CVC4JNI.Parser_getSymbolTable(swigCPtr, this);
    return (cPtr == 0) ? null : new SymbolTable(cPtr, false);
  }

  public boolean isOverloadedFunction(Expr fun) {
    return CVC4JNI.Parser_isOverloadedFunction(swigCPtr, this, Expr.getCPtr(fun), fun);
  }

  public Expr getOverloadedConstantForType(String name, Type t) {
    return new Expr(CVC4JNI.Parser_getOverloadedConstantForType(swigCPtr, this, name, Type.getCPtr(t), t), true);
  }

  public Expr getOverloadedFunctionForTypes(String name, vectorType argTypes) {
    return new Expr(CVC4JNI.Parser_getOverloadedFunctionForTypes(swigCPtr, this, name, vectorType.getCPtr(argTypes), argTypes), true);
  }

}
