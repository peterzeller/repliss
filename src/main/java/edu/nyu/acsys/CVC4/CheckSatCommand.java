/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class CheckSatCommand extends Command {
  private transient long swigCPtr;

  protected CheckSatCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.CheckSatCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(CheckSatCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_CheckSatCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public CheckSatCommand() {
    this(CVC4JNI.new_CheckSatCommand__SWIG_0(), true);
  }

  public CheckSatCommand(Expr expr, boolean inUnsatCore) {
    this(CVC4JNI.new_CheckSatCommand__SWIG_1(Expr.getCPtr(expr), expr, inUnsatCore), true);
  }

  public CheckSatCommand(Expr expr) {
    this(CVC4JNI.new_CheckSatCommand__SWIG_2(Expr.getCPtr(expr), expr), true);
  }

  public Expr getExpr() {
    return new Expr(CVC4JNI.CheckSatCommand_getExpr(swigCPtr, this), true);
  }

  public Result getResult() {
    return new Result(CVC4JNI.CheckSatCommand_getResult(swigCPtr, this), true);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.CheckSatCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public void printResult(java.io.OutputStream out, long verbosity) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.CheckSatCommand_printResult__SWIG_0(swigCPtr, this, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout), verbosity);
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public void printResult(java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.CheckSatCommand_printResult__SWIG_1(swigCPtr, this, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.CheckSatCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.CheckSatCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.CheckSatCommand_getCommandName(swigCPtr, this);
  }

}
