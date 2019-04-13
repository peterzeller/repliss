/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class DefineFunctionRecCommand extends Command {
  private transient long swigCPtr;

  protected DefineFunctionRecCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.DefineFunctionRecCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(DefineFunctionRecCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_DefineFunctionRecCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public DefineFunctionRecCommand(Expr func, vectorExpr formals, Expr formula) {
    this(CVC4JNI.new_DefineFunctionRecCommand__SWIG_0(Expr.getCPtr(func), func, vectorExpr.getCPtr(formals), formals, Expr.getCPtr(formula), formula), true);
  }

  public DefineFunctionRecCommand(vectorExpr funcs, vectorVectorExpr formals, vectorExpr formula) {
    this(CVC4JNI.new_DefineFunctionRecCommand__SWIG_1(vectorExpr.getCPtr(funcs), funcs, vectorVectorExpr.getCPtr(formals), formals, vectorExpr.getCPtr(formula), formula), true);
  }

  public vectorExpr getFunctions() {
    return new vectorExpr(CVC4JNI.DefineFunctionRecCommand_getFunctions(swigCPtr, this), false);
  }

  public vectorVectorExpr getFormals() {
    return new vectorVectorExpr(CVC4JNI.DefineFunctionRecCommand_getFormals(swigCPtr, this), false);
  }

  public vectorExpr getFormulas() {
    return new vectorExpr(CVC4JNI.DefineFunctionRecCommand_getFormulas(swigCPtr, this), false);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.DefineFunctionRecCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.DefineFunctionRecCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.DefineFunctionRecCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.DefineFunctionRecCommand_getCommandName(swigCPtr, this);
  }

}