/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class EchoCommand extends Command {
  private transient long swigCPtr;

  protected EchoCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.EchoCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(EchoCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_EchoCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public EchoCommand(String output) {
    this(CVC4JNI.new_EchoCommand__SWIG_0(output), true);
  }

  public EchoCommand() {
    this(CVC4JNI.new_EchoCommand__SWIG_1(), true);
  }

  public String getOutput() {
    return CVC4JNI.EchoCommand_getOutput(swigCPtr, this);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.EchoCommand_invoke__SWIG_0(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public void invoke(SmtEngine smtEngine, java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.EchoCommand_invoke__SWIG_1(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.EchoCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.EchoCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.EchoCommand_getCommandName(swigCPtr, this);
  }

}
