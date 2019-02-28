/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class BitVectorExtract {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected BitVectorExtract(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(BitVectorExtract obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_BitVectorExtract(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void setHigh(long value) {
    CVC4JNI.BitVectorExtract_high_set(swigCPtr, this, value);
  }

  public long getHigh() {
    return CVC4JNI.BitVectorExtract_high_get(swigCPtr, this);
  }

  public void setLow(long value) {
    CVC4JNI.BitVectorExtract_low_set(swigCPtr, this, value);
  }

  public long getLow() {
    return CVC4JNI.BitVectorExtract_low_get(swigCPtr, this);
  }

  public BitVectorExtract(long high, long low) {
    this(CVC4JNI.new_BitVectorExtract(high, low), true);
  }

  public boolean equals(BitVectorExtract extract) {
    return CVC4JNI.BitVectorExtract_equals(swigCPtr, this, BitVectorExtract.getCPtr(extract), extract);
  }

}
