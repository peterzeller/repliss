/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

class JavaIteratorAdapter_CommandSequence implements java.util.Iterator<Command> {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected JavaIteratorAdapter_CommandSequence(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(JavaIteratorAdapter_CommandSequence obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_JavaIteratorAdapter_CommandSequence(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void remove() {
    throw new UnsupportedOperationException();
  }

  public Command next() {
    if(hasNext()) {
      return getNext();
    } else {
      throw new java.util.NoSuchElementException();
    }
  }

  public JavaIteratorAdapter_CommandSequence(CommandSequence t) {
    this(CVC4JNI.new_JavaIteratorAdapter_CommandSequence(CommandSequence.getCPtr(t), t), true);
  }

  public boolean hasNext() {
    return CVC4JNI.JavaIteratorAdapter_CommandSequence_hasNext(swigCPtr, this);
  }

  private Command getNext() { return CVC4JNI.JavaIteratorAdapter_CommandSequence_getNext(swigCPtr, this); }

}
