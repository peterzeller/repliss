/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class PropagateRuleCommand extends Command {
  private transient long swigCPtr;

  protected PropagateRuleCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.PropagateRuleCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(PropagateRuleCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_PropagateRuleCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public PropagateRuleCommand(vectorExpr vars, vectorExpr guards, vectorExpr heads, Expr body, vectorVectorExpr d_triggers, boolean d_deduction) {
    this(CVC4JNI.new_PropagateRuleCommand__SWIG_0(vectorExpr.getCPtr(vars), vars, vectorExpr.getCPtr(guards), guards, vectorExpr.getCPtr(heads), heads, Expr.getCPtr(body), body, vectorVectorExpr.getCPtr(d_triggers), d_triggers, d_deduction), true);
  }

  public PropagateRuleCommand(vectorExpr vars, vectorExpr guards, vectorExpr heads, Expr body, vectorVectorExpr d_triggers) {
    this(CVC4JNI.new_PropagateRuleCommand__SWIG_1(vectorExpr.getCPtr(vars), vars, vectorExpr.getCPtr(guards), guards, vectorExpr.getCPtr(heads), heads, Expr.getCPtr(body), body, vectorVectorExpr.getCPtr(d_triggers), d_triggers), true);
  }

  public PropagateRuleCommand(vectorExpr vars, vectorExpr heads, Expr body, boolean d_deduction) {
    this(CVC4JNI.new_PropagateRuleCommand__SWIG_2(vectorExpr.getCPtr(vars), vars, vectorExpr.getCPtr(heads), heads, Expr.getCPtr(body), body, d_deduction), true);
  }

  public PropagateRuleCommand(vectorExpr vars, vectorExpr heads, Expr body) {
    this(CVC4JNI.new_PropagateRuleCommand__SWIG_3(vectorExpr.getCPtr(vars), vars, vectorExpr.getCPtr(heads), heads, Expr.getCPtr(body), body), true);
  }

  public vectorExpr getVars() {
    return new vectorExpr(CVC4JNI.PropagateRuleCommand_getVars(swigCPtr, this), false);
  }

  public vectorExpr getGuards() {
    return new vectorExpr(CVC4JNI.PropagateRuleCommand_getGuards(swigCPtr, this), false);
  }

  public vectorExpr getHeads() {
    return new vectorExpr(CVC4JNI.PropagateRuleCommand_getHeads(swigCPtr, this), false);
  }

  public Expr getBody() {
    return new Expr(CVC4JNI.PropagateRuleCommand_getBody(swigCPtr, this), true);
  }

  public vectorVectorExpr getTriggers() {
    return new vectorVectorExpr(CVC4JNI.PropagateRuleCommand_getTriggers(swigCPtr, this), false);
  }

  public boolean isDeduction() {
    return CVC4JNI.PropagateRuleCommand_isDeduction(swigCPtr, this);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.PropagateRuleCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.PropagateRuleCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.PropagateRuleCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.PropagateRuleCommand_getCommandName(swigCPtr, this);
  }

}
