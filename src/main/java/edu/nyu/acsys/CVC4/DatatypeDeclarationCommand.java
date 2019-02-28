/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class DatatypeDeclarationCommand extends Command {
  private transient long swigCPtr;

  protected DatatypeDeclarationCommand(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.DatatypeDeclarationCommand_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(DatatypeDeclarationCommand obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_DatatypeDeclarationCommand(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public DatatypeDeclarationCommand(DatatypeType datatype) {
    this(CVC4JNI.new_DatatypeDeclarationCommand__SWIG_0(DatatypeType.getCPtr(datatype), datatype), true);
  }

  public DatatypeDeclarationCommand(vectorDatatypeType datatypes) {
    this(CVC4JNI.new_DatatypeDeclarationCommand__SWIG_1(vectorDatatypeType.getCPtr(datatypes), datatypes), true);
  }

  public vectorDatatypeType getDatatypes() {
    return new vectorDatatypeType(CVC4JNI.DatatypeDeclarationCommand_getDatatypes(swigCPtr, this), false);
  }

  public void invoke(SmtEngine smtEngine) {
    CVC4JNI.DatatypeDeclarationCommand_invoke(swigCPtr, this, SmtEngine.getCPtr(smtEngine), smtEngine);
  }

  public Command exportTo(ExprManager exprManager, ExprManagerMapCollection variableMap) {
    long cPtr = CVC4JNI.DatatypeDeclarationCommand_exportTo(swigCPtr, this, ExprManager.getCPtr(exprManager), exprManager, ExprManagerMapCollection.getCPtr(variableMap), variableMap);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public Command clone() {
    long cPtr = CVC4JNI.DatatypeDeclarationCommand_clone(swigCPtr, this);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public String getCommandName() {
    return CVC4JNI.DatatypeDeclarationCommand_getCommandName(swigCPtr, this);
  }

}
