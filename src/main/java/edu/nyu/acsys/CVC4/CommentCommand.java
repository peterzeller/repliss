/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class CommentCommand extends Command {
  private transient long swigCPtr;

  protected CommentCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.CommentCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(CommentCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_CommentCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public CommentCommand(String comment) {
    this(CVC4JNI.new_CommentCommand(comment), true);
  }

  public String getComment() {
    return CVC4JNI.CommentCommand_getComment(swigCPtr, this);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.CommentCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.CommentCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.CommentCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.CommentCommand_getCommandName(swigCPtr, this);
  }

}
