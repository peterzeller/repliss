/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class BitVector {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected BitVector(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(BitVector obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_BitVector(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public BitVector(long size, Integer val) {
    this(CVC4JNI.new_BitVector__SWIG_0(size, Integer.getCPtr(val), val), true);
  }

  public BitVector(long size) {
    this(CVC4JNI.new_BitVector__SWIG_1(size), true);
  }

  public BitVector() {
    this(CVC4JNI.new_BitVector__SWIG_2(), true);
  }

  public BitVector(long size, long z) {
    this(CVC4JNI.new_BitVector__SWIG_3(size, z), true);
  }

  public BitVector(long size, java.math.BigInteger z) {
    this(CVC4JNI.new_BitVector__SWIG_4(size, z), true);
  }

  public BitVector(long size, BitVector q) {
    this(CVC4JNI.new_BitVector__SWIG_5(size, BitVector.getCPtr(q), q), true);
  }

  public BitVector(String num, long base) {
    this(CVC4JNI.new_BitVector__SWIG_6(num, base), true);
  }

  public BitVector(String num) {
    this(CVC4JNI.new_BitVector__SWIG_7(num), true);
  }

  public BitVector assign(BitVector x) {
    return new BitVector(CVC4JNI.BitVector_assign(swigCPtr, this, BitVector.getCPtr(x), x), false);
  }

  public long getSize() {
    return CVC4JNI.BitVector_getSize(swigCPtr, this);
  }

  public Integer getValue() {
    return new Integer(CVC4JNI.BitVector_getValue(swigCPtr, this), false);
  }

  public Integer toInteger() {
    return new Integer(CVC4JNI.BitVector_toInteger(swigCPtr, this), true);
  }

  public Integer toSignedInteger() {
    return new Integer(CVC4JNI.BitVector_toSignedInteger(swigCPtr, this), true);
  }

  public String toString(long base) {
    return CVC4JNI.BitVector_toString__SWIG_0(swigCPtr, this, base);
  }

  public String toString() {
    return CVC4JNI.BitVector_toString__SWIG_1(swigCPtr, this);
  }

  public long hash() {
    return CVC4JNI.BitVector_hash(swigCPtr, this);
  }

  public BitVector setBit(long i) {
    return new BitVector(CVC4JNI.BitVector_setBit(swigCPtr, this, i), true);
  }

  public boolean isBitSet(long i) {
    return CVC4JNI.BitVector_isBitSet(swigCPtr, this, i);
  }

  public long isPow2() {
    return CVC4JNI.BitVector_isPow2(swigCPtr, this);
  }

  public BitVector concat(BitVector other) {
    return new BitVector(CVC4JNI.BitVector_concat(swigCPtr, this, BitVector.getCPtr(other), other), true);
  }

  public BitVector extract(long high, long low) {
    return new BitVector(CVC4JNI.BitVector_extract(swigCPtr, this, high, low), true);
  }

  public boolean equals(BitVector y) {
    return CVC4JNI.BitVector_equals(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean less(BitVector y) {
    return CVC4JNI.BitVector_less(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean lessEqual(BitVector y) {
    return CVC4JNI.BitVector_lessEqual(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean greater(BitVector y) {
    return CVC4JNI.BitVector_greater(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean greaterEqual(BitVector y) {
    return CVC4JNI.BitVector_greaterEqual(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean unsignedLessThan(BitVector y) {
    return CVC4JNI.BitVector_unsignedLessThan(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean unsignedLessThanEq(BitVector y) {
    return CVC4JNI.BitVector_unsignedLessThanEq(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean signedLessThan(BitVector y) {
    return CVC4JNI.BitVector_signedLessThan(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public boolean signedLessThanEq(BitVector y) {
    return CVC4JNI.BitVector_signedLessThanEq(swigCPtr, this, BitVector.getCPtr(y), y);
  }

  public BitVector bitXor(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_bitXor(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector bitOr(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_bitOr(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector bitAnd(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_bitAnd(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector complement() {
    return new BitVector(CVC4JNI.BitVector_complement(swigCPtr, this), true);
  }

  public BitVector plus(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_plus(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector minus(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_minus__SWIG_0(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector minus() {
    return new BitVector(CVC4JNI.BitVector_minus__SWIG_1(swigCPtr, this), true);
  }

  public BitVector times(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_times(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector unsignedDivTotal(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_unsignedDivTotal(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector unsignedRemTotal(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_unsignedRemTotal(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector zeroExtend(long n) {
    return new BitVector(CVC4JNI.BitVector_zeroExtend(swigCPtr, this, n), true);
  }

  public BitVector signExtend(long n) {
    return new BitVector(CVC4JNI.BitVector_signExtend(swigCPtr, this, n), true);
  }

  public BitVector leftShift(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_leftShift(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector logicalRightShift(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_logicalRightShift(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public BitVector arithRightShift(BitVector y) {
    return new BitVector(CVC4JNI.BitVector_arithRightShift(swigCPtr, this, BitVector.getCPtr(y), y), true);
  }

  public static BitVector mkOnes(long size) {
    return new BitVector(CVC4JNI.BitVector_mkOnes(size), true);
  }

  public static BitVector mkMinSigned(long size) {
    return new BitVector(CVC4JNI.BitVector_mkMinSigned(size), true);
  }

  public static BitVector mkMaxSigned(long size) {
    return new BitVector(CVC4JNI.BitVector_mkMaxSigned(size), true);
  }

}
