/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class QueryCommand extends Command {
  private transient long swigCPtr;

  protected QueryCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.QueryCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(QueryCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_QueryCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public QueryCommand(Expr e, boolean inUnsatCore) {
    this(CVC4JNI.new_QueryCommand__SWIG_0(Expr.getCPtr(e), e, inUnsatCore), true);
  }

  public QueryCommand(Expr e) {
    this(CVC4JNI.new_QueryCommand__SWIG_1(Expr.getCPtr(e), e), true);
  }

  public Expr getExpr() {
    return new Expr(CVC4JNI.QueryCommand_getExpr(swigCPtr, this), true);
  }

  public Result getResult() {
    return new Result(CVC4JNI.QueryCommand_getResult(swigCPtr, this), true);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.QueryCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public void printResult(java.io.OutputStream out, long verbosity) {
    JavaOutputStreamAdapter tempout = new JavaOutputStreamAdapter();
    try {
      CVC4JNI.QueryCommand_printResult__SWIG_0(swigCPtr, this, JavaOutputStreamAdapter.getCPtr(tempout), verbosity);
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public void printResult(java.io.OutputStream out) {
    JavaOutputStreamAdapter tempout = new JavaOutputStreamAdapter();
    try {
      CVC4JNI.QueryCommand_printResult__SWIG_1(swigCPtr, this, JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.QueryCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.QueryCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.QueryCommand_getCommandName(swigCPtr, this);
  }

}
