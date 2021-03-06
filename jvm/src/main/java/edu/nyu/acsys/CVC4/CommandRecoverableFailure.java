/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class CommandRecoverableFailure extends CommandStatus {
  private transient long swigCPtr;

  protected CommandRecoverableFailure(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.CommandRecoverableFailure_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(CommandRecoverableFailure obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_CommandRecoverableFailure(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public CommandRecoverableFailure(String message) {
    this(CVC4JNI.new_CommandRecoverableFailure(message), true);
  }

  public CommandStatus clone() {
    return new CommandRecoverableFailure(CVC4JNI.CommandRecoverableFailure_clone(swigCPtr, this), false);
  }

  public String getMessage() {
    return CVC4JNI.CommandRecoverableFailure_getMessage(swigCPtr, this);
  }

}
