/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class ParserEndOfFileException extends ParserException {
  private transient long swigCPtr;

  protected ParserEndOfFileException(long cPtr, boolean cMemoryOwn) {
    super(CVC4JNI.ParserEndOfFileException_SWIGUpcast(cPtr), cMemoryOwn);
    swigCPtr = cPtr;
  }

  protected static long getCPtr(ParserEndOfFileException obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_ParserEndOfFileException(swigCPtr);
      }
      swigCPtr = 0;
    }
    super.delete();
  }

  public ParserEndOfFileException() {
    this(CVC4JNI.new_ParserEndOfFileException__SWIG_0(), true);
  }

  public ParserEndOfFileException(String msg) {
    this(CVC4JNI.new_ParserEndOfFileException__SWIG_1(msg), true);
  }

  public ParserEndOfFileException(String msg, String filename, long line, long column) {
    this(CVC4JNI.new_ParserEndOfFileException__SWIG_2(msg, filename, line, column), true);
  }

}
