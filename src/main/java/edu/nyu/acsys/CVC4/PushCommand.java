/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class PushCommand extends Command {
  private transient long swigCPtr;

  protected PushCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.PushCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(PushCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_PushCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.PushCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.PushCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.PushCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.PushCommand_getCommandName(swigCPtr, this);
  }

  public PushCommand() {
    this(CVC4JNI.new_PushCommand(), true);
  }

}
