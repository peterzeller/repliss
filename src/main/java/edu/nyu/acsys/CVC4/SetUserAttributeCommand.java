/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class SetUserAttributeCommand extends Command {
  private transient long swigCPtr;

  protected SetUserAttributeCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.SetUserAttributeCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(SetUserAttributeCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_SetUserAttributeCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public SetUserAttributeCommand(String attr, Expr expr) {
    this(CVC4JNI.new_SetUserAttributeCommand__SWIG_0(attr, Expr.getCPtr(expr), expr), true);
  }

  public SetUserAttributeCommand(String attr, Expr expr, vectorExpr values) {
    this(CVC4JNI.new_SetUserAttributeCommand__SWIG_1(attr, Expr.getCPtr(expr), expr, vectorExpr.getCPtr(values), values), true);
  }

  public SetUserAttributeCommand(String attr, Expr expr, String value) {
    this(CVC4JNI.new_SetUserAttributeCommand__SWIG_2(attr, Expr.getCPtr(expr), expr, value), true);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.SetUserAttributeCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.SetUserAttributeCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.SetUserAttributeCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.SetUserAttributeCommand_getCommandName(swigCPtr, this);
  }

}
