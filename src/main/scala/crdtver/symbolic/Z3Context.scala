package crdtver.symbolic

import com.microsoft.z3
import com.microsoft.z3._
import com.microsoft.z3.enumerations.Z3_ast_print_mode

trait Z3Context extends AutoCloseable {
  def mkLambda(sorts: Array[Sort], symbols: Array[Symbol], expr: BoolExpr): Expr

  /**
    * Creates a new symbol using an integer.
    * Remarks: Not all integers can be passed to this function.
    * The legal range of unsigned integers is 0 to 2^30-1.
    **/
  def mkSymbol(i: Int): IntSymbol

  /**
    * Create a symbol using a string.
    **/
  def mkSymbol(name: String): StringSymbol

  /**
    * Retrieves the Boolean sort of the context.
    **/
  def getBoolSort: BoolSort

  /**
    * Retrieves the Integer sort of the context.
    **/
  def getIntSort: IntSort

  /**
    * Retrieves the Real sort of the context.
    **/
  def getRealSort: RealSort

  /**
    * Create a new Boolean sort.
    **/
  def mkBoolSort: BoolSort

  def getStringSort: SeqSort

  /**
    * Create a new uninterpreted sort.
    **/
  def mkUninterpretedSort(s: Symbol): UninterpretedSort

  def mkUninterpretedSort(str: String): UninterpretedSort

  /**
    * Create a new integer sort.
    **/
  def mkIntSort: IntSort

  /**
    * Create a real sort.
    **/
  def mkRealSort: RealSort

  /**
    * Create a new bit-vector sort.
    **/
  def mkBitVecSort(size: Int): BitVecSort

  /**
    * Create a new array sort.
    **/
  def mkArraySort(domain: Sort, range: Sort): ArraySort

  /**
    * Create a new string sort
    **/
  def mkStringSort: SeqSort

  /**
    * Create a new sequence sort
    **/
  def mkSeqSort(s: Sort): SeqSort

  /**
    * Create a new regular expression sort
    **/
  def mkReSort(s: Sort): ReSort

  /**
    * Create a new tuple sort.
    **/
  def mkTupleSort(name: Symbol, fieldNames: Array[Symbol], fieldSorts: Array[Sort]): TupleSort

  /**
    * Create a new enumeration sort.
    **/
  def mkEnumSort(name: Symbol, enumNames: Symbol*): EnumSort

  def mkEnumSort(name: String, enumNames: String*): EnumSort

  /**
    * Create a new list sort.
    **/
  def mkListSort(name: Symbol, elemSort: Sort): ListSort

  def mkListSort(name: String, elemSort: Sort): ListSort

  /**
    * Create a new finite domain sort.
    **/
  def mkFiniteDomainSort(name: Symbol, size: Long): FiniteDomainSort

  def mkFiniteDomainSort(name: String, size: Long): FiniteDomainSort

  /**
    * Create a datatype constructor.
    *
    * @param name       constructor name
    * @param recognizer name of recognizer function.
    * @param fieldNames names of the constructor fields.
    * @param sorts      field sorts, 0 if the field sort refers to a recursive sort.
    * @param sortRefs   reference to datatype sort that is an argument to the
    *                   constructor; if the corresponding sort reference is 0, then the value in sort_refs should be
    *                   an index referring to one of the recursive datatypes that is
    *                   declared.
    **/
  def mkConstructor(name: Symbol, recognizer: Symbol, fieldNames: Array[Symbol], sorts: Array[Sort], sortRefs: Array[Int]): Constructor

  /**
    * Create a datatype constructor.
    **/
  def mkConstructor(name: String, recognizer: String, fieldNames: Array[String], sorts: Array[Sort], sortRefs: Array[Int]): Constructor

  /**
    * Create a new datatype sort.
    **/
  def mkDatatypeSort(name: Symbol, constructors: Array[Constructor]): DatatypeSort

  def mkDatatypeSort(name: String, constructors: Array[Constructor]): DatatypeSort

  /**
    * Create mutually recursive datatypes.
    *
    * @param names names of datatype sorts
    * @param c     list of constructors, one list per sort.
    **/
  def mkDatatypeSorts(names: Array[Symbol], c: Array[Array[Constructor]]): Array[DatatypeSort]

  /**
    * Create mutually recursive data-types.
    **/
  def mkDatatypeSorts(names: Array[String], c: Array[Array[Constructor]]): Array[DatatypeSort]

  /**
    * Update a datatype field at expression t with value v.
    * The function performs a record update at t. The field
    * that is passed in as argument is updated with value v,
    * the remaining fields of t are unchanged.
    **/
  @throws[Z3Exception]
  def MkUpdateField(field: FuncDecl, t: Expr, v: Expr): Expr

  /**
    * Creates a new function declaration.
    **/
  def mkFuncDecl(name: Symbol, domain: Array[Sort], range: Sort): FuncDecl

  def mkFuncDecl(name: Symbol, domain: Sort, range: Sort): FuncDecl

  def mkFuncDecl(name: String, domain: Array[Sort], range: Sort): FuncDecl

  def mkFuncDecl(name: String, domain: Sort, range: Sort): FuncDecl

  /**
    * Creates a fresh function declaration with a name prefixed with
    * {@code prefix}.
    *
    * @see #mkFuncDecl(String,Sort,Sort)
    * @see #mkFuncDecl(String,Sort[],Sort)
    **/
  def mkFreshFuncDecl(prefix: String, domain: Array[Sort], range: Sort): FuncDecl

  /**
    * Creates a new constant function declaration.
    **/
  def mkConstDecl(name: Symbol, range: Sort): FuncDecl

  def mkConstDecl(name: String, range: Sort): FuncDecl

  /**
    * Creates a fresh constant function declaration with a name prefixed with
    * {@code prefix"}.
    *
    * @see #mkFuncDecl(String,Sort,Sort)
    * @see #mkFuncDecl(String,Sort[],Sort)
    **/
  def mkFreshConstDecl(prefix: String, range: Sort): FuncDecl

  /**
    * Creates a new bound variable.
    *
    * @param index The de-Bruijn index of the variable
    * @param ty    The sort of the variable
    **/
  def mkBound(index: Int, ty: Sort): Expr

  /**
    * Create a quantifier pattern.
    **/
  def mkPattern(terms: Expr*): Pattern

  /**
    * Creates a new Constant of sort {@code range} and named
    * {@code name}.
    **/
  def mkConst(name: Symbol, range: Sort): Expr

  def mkConst(name: String, range: Sort): Expr

  /**
    * Creates a fresh Constant of sort {@code range} and a name
    * prefixed with {@code prefix}.
    **/
  def mkFreshConst(prefix: String, range: Sort): Expr

  /**
    * Creates a fresh constant from the FuncDecl {@code f}.
    *
    * @param f A decl of a 0-arity function
    **/
  def mkConst(f: FuncDecl): Expr

  /**
    * Create a Boolean constant.
    **/
  def mkBoolConst(name: Symbol): BoolExpr

  def mkBoolConst(name: String): BoolExpr

  /**
    * Creates an integer constant.
    **/
  def mkIntConst(name: Symbol): IntExpr

  def mkIntConst(name: String): IntExpr

  /**
    * Creates a real constant.
    **/
  def mkRealConst(name: Symbol): RealExpr

  def mkRealConst(name: String): RealExpr

  /**
    * Creates a bit-vector constant.
    **/
  def mkBVConst(name: Symbol, size: Int): BitVecExpr

  def mkBVConst(name: String, size: Int): BitVecExpr

  /**
    * Create a new function application.
    **/
  def mkApp(f: FuncDecl, args: Expr*): Expr

  /**
    * The true Term.
    **/
  def mkTrue(): BoolExpr

  /**
    * The false Term.
    **/
  def mkFalse: BoolExpr

  /**
    * Creates a Boolean value.
    **/
  def mkBool(value: Boolean): BoolExpr

  /**
    * Creates the equality {@code x"/> = <paramref name="y}.
    **/
  def mkEq(x: Expr, y: Expr): BoolExpr

  /**
    * Creates a {@code distinct} term.
    **/
  def mkDistinct(args: Expr*): BoolExpr

  /**
    * Mk an expression representing {@code not(a)}.
    **/
  def mkNot(a: BoolExpr): BoolExpr

  /**
    * Create an expression representing an if-then-else:
    * {@code ite(t1, t2, t3)}.
    *
    * @param t1 An expression with Boolean sort
    * @param t2 An expression
    * @param t3 An expression with the same sort as { @code t2}
    **/
  def mkITE(t1: BoolExpr, t2: Expr, t3: Expr): Expr

  /**
    * Create an expression representing {@code t1 iff t2}.
    **/
  def mkIff(t1: BoolExpr, t2: BoolExpr): BoolExpr

  /**
    * Create an expression representing {@code t1 -> t2}.
    **/
  def mkImplies(t1: BoolExpr, t2: BoolExpr): BoolExpr

  /**
    * Create an expression representing {@code t1 xor t2}.
    **/
  def mkXor(t1: BoolExpr, t2: BoolExpr): BoolExpr

  /**
    * Create an expression representing {@code t[0] and t[1] and ...}.
    **/
  def mkAnd(t: BoolExpr*): BoolExpr

  /**
    * Create an expression representing {@code t[0] or t[1] or ...}.
    **/
  def mkOr(t: BoolExpr*): BoolExpr

  /**
    * Create an expression representing {@code t[0] + t[1] + ...}.
    **/
  def mkAdd(t: ArithExpr*): ArithExpr

  /**
    * Create an expression representing {@code t[0] * t[1] * ...}.
    **/
  def mkMul(t: ArithExpr*): ArithExpr

  /**
    * Create an expression representing {@code t[0] - t[1] - ...}.
    **/
  def mkSub(t: ArithExpr*): ArithExpr

  /**
    * Create an expression representing {@code -t}.
    **/
  def mkUnaryMinus(t: ArithExpr): ArithExpr

  /**
    * Create an expression representing {@code t1 / t2}.
    **/
  def mkDiv(t1: ArithExpr, t2: ArithExpr): ArithExpr

  /**
    * Create an expression representing {@code t1 mod t2}.
    * Remarks: The
    * arguments must have int type.
    **/
  def mkMod(t1: IntExpr, t2: IntExpr): IntExpr

  /**
    * Create an expression representing {@code t1 rem t2}.
    * Remarks: The
    * arguments must have int type.
    **/
  def mkRem(t1: IntExpr, t2: IntExpr): IntExpr

  /**
    * Create an expression representing {@code t1 ^ t2}.
    **/
  def mkPower(t1: ArithExpr, t2: ArithExpr): ArithExpr

  /**
    * Create an expression representing {@code t1 &lt; t2}
    **/
  def mkLt(t1: ArithExpr, t2: ArithExpr): BoolExpr

  /**
    * Create an expression representing {@code t1 &lt;= t2}
    **/
  def mkLe(t1: ArithExpr, t2: ArithExpr): BoolExpr

  /**
    * Create an expression representing {@code t1 &gt; t2}
    **/
  def mkGt(t1: ArithExpr, t2: ArithExpr): BoolExpr

  /**
    * Create an expression representing {@code t1 &gt;= t2}
    **/
  def mkGe(t1: ArithExpr, t2: ArithExpr): BoolExpr

  /**
    * Coerce an integer to a real.
    * Remarks:  There is also a converse operation
    * exposed. It follows the semantics prescribed by the SMT-LIB standard.
    *
    * You can take the floor of a real by creating an auxiliary integer Term
    * {@code k} and and asserting
    * {@code MakeInt2Real(k) &lt;= t1 &lt; MkInt2Real(k)+1}. The argument
    * must be of integer sort.
    **/
  def mkInt2Real(t: IntExpr): RealExpr

  /**
    * Coerce a real to an integer.
    * Remarks:  The semantics of this function
    * follows the SMT-LIB standard for the function to_int. The argument must
    * be of real sort.
    **/
  def mkReal2Int(t: RealExpr): IntExpr

  /**
    * Creates an expression that checks whether a real number is an integer.
    **/
  def mkIsInteger(t: RealExpr): BoolExpr

  /**
    * Bitwise negation.
    * Remarks: The argument must have a bit-vector
    * sort.
    **/
  def mkBVNot(t: BitVecExpr): BitVecExpr

  /**
    * Take conjunction of bits in a vector, return vector of length 1.
    *
    * Remarks: The argument must have a bit-vector sort.
    **/
  def mkBVRedAND(t: BitVecExpr): BitVecExpr

  /**
    * Take disjunction of bits in a vector, return vector of length 1.
    *
    * Remarks: The argument must have a bit-vector sort.
    **/
  def mkBVRedOR(t: BitVecExpr): BitVecExpr

  /**
    * Bitwise conjunction.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  def mkBVAND(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Bitwise disjunction.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  def mkBVOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Bitwise XOR.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  def mkBVXOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Bitwise NAND.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  def mkBVNAND(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Bitwise NOR.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  def mkBVNOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Bitwise XNOR.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  def mkBVXNOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Standard two's complement unary minus.
    * Remarks: The arguments must have a
    * bit-vector sort.
    **/
  def mkBVNeg(t: BitVecExpr): BitVecExpr

  /**
    * Two's complement addition.
    * Remarks: The arguments must have the same
    * bit-vector sort.
    **/
  def mkBVAdd(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Two's complement subtraction.
    * Remarks: The arguments must have the same
    * bit-vector sort.
    **/
  def mkBVSub(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Two's complement multiplication.
    * Remarks: The arguments must have the
    * same bit-vector sort.
    **/
  def mkBVMul(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Unsigned division.
    * Remarks:  It is defined as the floor of
    * {@code t1/t2} if \c t2 is different from zero. If {@code t2} is
    * zero, then the result is undefined. The arguments must have the same
    * bit-vector sort.
    **/
  def mkBVUDiv(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Signed division.
    * Remarks:  It is defined in the following way:
    *
    * - The \c floor of {@code t1/t2} if \c t2 is different from zero, and
    * {@code t1*t2 >= 0}.
    *
    * - The \c ceiling of {@code t1/t2} if \c t2 is different from zero,
    * and {@code t1*t2 &lt; 0}.
    *
    * If {@code t2} is zero, then the result is undefined. The arguments
    * must have the same bit-vector sort.
    **/
  def mkBVSDiv(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Unsigned remainder.
    * Remarks:  It is defined as
    * {@code t1 - (t1 /u t2) * t2}, where {@code /u} represents
    * unsigned division. If {@code t2} is zero, then the result is
    * undefined. The arguments must have the same bit-vector sort.
    **/
  def mkBVURem(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Signed remainder.
    * Remarks:  It is defined as
    * {@code t1 - (t1 /s t2) * t2}, where {@code /s} represents
    * signed division. The most significant bit (sign) of the result is equal
    * to the most significant bit of \c t1.
    *
    * If {@code t2} is zero, then the result is undefined. The arguments
    * must have the same bit-vector sort.
    **/
  def mkBVSRem(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Two's complement signed remainder (sign follows divisor).
    * Remarks:  If
    * {@code t2} is zero, then the result is undefined. The arguments must
    * have the same bit-vector sort.
    **/
  def mkBVSMod(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Unsigned less-than
    * Remarks:  The arguments must have the same bit-vector
    * sort.
    **/
  def mkBVULT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Two's complement signed less-than
    * Remarks:  The arguments must have the
    * same bit-vector sort.
    **/
  def mkBVSLT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Unsigned less-than or equal to.
    * Remarks:  The arguments must have the
    * same bit-vector sort.
    **/
  def mkBVULE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Two's complement signed less-than or equal to.
    * Remarks:  The arguments
    * must have the same bit-vector sort.
    **/
  def mkBVSLE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Unsigned greater than or equal to.
    * Remarks:  The arguments must have the
    * same bit-vector sort.
    **/
  def mkBVUGE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Two's complement signed greater than or equal to.
    * Remarks:  The arguments
    * must have the same bit-vector sort.
    **/
  def mkBVSGE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Unsigned greater-than.
    * Remarks:  The arguments must have the same
    * bit-vector sort.
    **/
  def mkBVUGT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Two's complement signed greater-than.
    * Remarks:  The arguments must have
    * the same bit-vector sort.
    **/
  def mkBVSGT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Bit-vector concatenation.
    * Remarks:  The arguments must have a bit-vector
    * sort.
    *
    * @return The result is a bit-vector of size { @code n1+n2}, where
    *         { @code n1} ({ @code n2}) is the size of { @code t1}
    *         ({ @code t2}).
    *
    **/
  def mkConcat(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Bit-vector extraction.
    * Remarks:  Extract the bits {@code high}
    * down to {@code low} from a bitvector of size {@code m} to
    * yield a new bitvector of size {@code n}, where
    * {@code n = high - low + 1}. The argument {@code t} must
    * have a bit-vector sort.
    **/
  def mkExtract(high: Int, low: Int, t: BitVecExpr): BitVecExpr

  /**
    * Bit-vector sign extension.
    * Remarks:  Sign-extends the given bit-vector to
    * the (signed) equivalent bitvector of size {@code m+i}, where \c m is
    * the size of the given bit-vector. The argument {@code t} must
    * have a bit-vector sort.
    **/
  def mkSignExt(i: Int, t: BitVecExpr): BitVecExpr

  /**
    * Bit-vector zero extension.
    * Remarks:  Extend the given bit-vector with
    * zeros to the (unsigned) equivalent bitvector of size {@code m+i},
    * where \c m is the size of the given bit-vector. The argument {@code t}
    * must have a bit-vector sort.
    **/
  def mkZeroExt(i: Int, t: BitVecExpr): BitVecExpr

  /**
    * Bit-vector repetition.
    * Remarks:  The argument {@code t} must
    * have a bit-vector sort.
    **/
  def mkRepeat(i: Int, t: BitVecExpr): BitVecExpr

  /**
    * Shift left.
    * Remarks:  It is equivalent to multiplication by
    * {@code 2^x} where \c x is the value of {@code t2}.
    *
    * NB. The semantics of shift operations varies between environments. This
    * definition does not necessarily capture directly the semantics of the
    * programming language or assembly architecture you are modeling.
    *
    * The arguments must have a bit-vector sort.
    **/
  def mkBVSHL(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Logical shift right
    * Remarks:  It is equivalent to unsigned division by
    * {@code 2^x} where \c x is the value of {@code t2}.
    *
    * NB. The semantics of shift operations varies between environments. This
    * definition does not necessarily capture directly the semantics of the
    * programming language or assembly architecture you are modeling.
    *
    * The arguments must have a bit-vector sort.
    **/
  def mkBVLSHR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Arithmetic shift right
    * Remarks:  It is like logical shift right except
    * that the most significant bits of the result always copy the most
    * significant bit of the second argument.
    *
    * NB. The semantics of shift operations varies between environments. This
    * definition does not necessarily capture directly the semantics of the
    * programming language or assembly architecture you are modeling.
    *
    * The arguments must have a bit-vector sort.
    **/
  def mkBVASHR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Rotate Left.
    * Remarks:  Rotate bits of \c t to the left \c i times. The
    * argument {@code t} must have a bit-vector sort.
    **/
  def mkBVRotateLeft(i: Int, t: BitVecExpr): BitVecExpr

  /**
    * Rotate Right.
    * Remarks:  Rotate bits of \c t to the right \c i times. The
    * argument {@code t} must have a bit-vector sort.
    **/
  def mkBVRotateRight(i: Int, t: BitVecExpr): BitVecExpr

  /**
    * Rotate Left.
    * Remarks:  Rotate bits of {@code t1} to the left
    * {@code t2} times. The arguments must have the same bit-vector
    * sort.
    **/
  def mkBVRotateLeft(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Rotate Right.
    * Remarks:  Rotate bits of {@code t1} to the
    * right{@code t2} times. The arguments must have the same
    * bit-vector sort.
    **/
  def mkBVRotateRight(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr

  /**
    * Create an {@code n} bit bit-vector from the integer argument
    * {@code t}.
    * Remarks:  NB. This function is essentially treated
    * as uninterpreted. So you cannot expect Z3 to precisely reflect the
    * semantics of this function when solving constraints with this function.
    *
    * The argument must be of integer sort.
    **/
  def mkInt2BV(n: Int, t: IntExpr): BitVecExpr

  /**
    * Create an integer from the bit-vector argument {@code t}.
    * Remarks:  If \c is_signed is false, then the bit-vector \c t1 is treated
    * as unsigned. So the result is non-negative and in the range
    * {@code [0..2^N-1]}, where N are the number of bits in {@code t}.
    * If \c is_signed is true, \c t1 is treated as a signed
    * bit-vector.
    *
    * NB. This function is essentially treated as uninterpreted. So you cannot
    * expect Z3 to precisely reflect the semantics of this function when
    * solving constraints with this function.
    *
    * The argument must be of bit-vector sort.
    **/
  def mkBV2Int(t: BitVecExpr, signed: Boolean): IntExpr

  /**
    * Create a predicate that checks that the bit-wise addition does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVAddNoOverflow(t1: BitVecExpr, t2: BitVecExpr, isSigned: Boolean): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise addition does not
    * underflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVAddNoUnderflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise subtraction does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVSubNoOverflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise subtraction does not
    * underflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVSubNoUnderflow(t1: BitVecExpr, t2: BitVecExpr, isSigned: Boolean): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise signed division does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVSDivNoOverflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise negation does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVNegNoOverflow(t: BitVecExpr): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise multiplication does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVMulNoOverflow(t1: BitVecExpr, t2: BitVecExpr, isSigned: Boolean): BoolExpr

  /**
    * Create a predicate that checks that the bit-wise multiplication does not
    * underflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  def mkBVMulNoUnderflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr

  /**
    * Create an array constant.
    **/
  def mkArrayConst(name: Symbol, domain: Sort, range: Sort): ArrayExpr

  def mkArrayConst(name: String, domain: Sort, range: Sort): ArrayExpr

  /**
    * Array read.
    * Remarks:  The argument {@code a} is the array and
    * {@code i} is the index of the array that gets read.
    *
    * The node {@code a} must have an array sort
    * {@code [domain -> range]}, and {@code i} must have the sort
    * {@code domain}. The sort of the result is {@code range}.
    *
    * @see #mkArraySort
    * @see #mkStore
    *
    **/
  def mkSelect(a: ArrayExpr, i: Expr): Expr

  /**
    * Array update.
    * Remarks:  The node {@code a} must have an array sort
    * {@code [domain -> range]}, {@code i} must have sort
    * {@code domain}, {@code v} must have sort range. The sort of the
    * result is {@code [domain -> range]}. The semantics of this function
    * is given by the theory of arrays described in the SMT-LIB standard. See
    * http://smtlib.org for more details. The result of this function is an
    * array that is equal to {@code a} (with respect to
    * {@code select}) on all indices except for {@code i}, where it
    * maps to {@code v} (and the {@code select} of {@code a}
    * with respect to {@code i} may be a different value).
    *
    * @see #mkArraySort
    * @see #mkSelect
    *
    **/
  def mkStore(a: ArrayExpr, i: Expr, v: Expr): ArrayExpr

  /**
    * Create a constant array.
    * Remarks:  The resulting term is an array, such
    * that a {@code select} on an arbitrary index produces the value
    * {@code v}.
    *
    * @see #mkArraySort
    * @see #mkSelect
    *
    **/
  def mkConstArray(domain: Sort, v: Expr): ArrayExpr

  /**
    * Maps f on the argument arrays.
    * Remarks:  Each element of
    * {@code args} must be of an array sort
    * {@code [domain_i -> range_i]}. The function declaration
    * {@code f} must have type {@code range_1 .. range_n -> range}.
    * {@code v} must have sort range. The sort of the result is
    * {@code [domain_i -> range]}.
    *
    * @see #mkArraySort
    * @see #mkSelect
    * @see #mkStore
    *
    **/
  def mkMap(f: FuncDecl, args: ArrayExpr*): ArrayExpr

  /**
    * Access the array default value.
    * Remarks:  Produces the default range
    * value, for arrays that can be represented as finite maps with a default
    * range value.
    **/
  def mkTermArray(array: ArrayExpr): Expr

  /**
    * Create Extentionality index. Two arrays are equal if and only if they are equal on the index returned by MkArrayExt.
    **/
  def mkArrayExt(arg1: ArrayExpr, arg2: ArrayExpr): Expr

  /**
    * Create a set type.
    **/
  def mkSetSort(ty: Sort): SetSort

  /**
    * Create an empty set.
    **/
  def mkEmptySet(domain: Sort): ArrayExpr

  /**
    * Create the full set.
    **/
  def mkFullSet(domain: Sort): ArrayExpr

  /**
    * Add an element to the set.
    **/
  def mkSetAdd(set: ArrayExpr, element: Expr): ArrayExpr

  /**
    * Remove an element from a set.
    **/
  def mkSetDel(set: ArrayExpr, element: Expr): ArrayExpr

  /**
    * Take the union of a list of sets.
    **/
  def mkSetUnion(args: ArrayExpr*): ArrayExpr

  /**
    * Take the intersection of a list of sets.
    **/
  def mkSetIntersection(args: ArrayExpr*): ArrayExpr

  /**
    * Take the difference between two sets.
    **/
  def mkSetDifference(arg1: ArrayExpr, arg2: ArrayExpr): ArrayExpr

  /**
    * Take the complement of a set.
    **/
  def mkSetComplement(arg: ArrayExpr): ArrayExpr

  /**
    * Check for set membership.
    **/
  def mkSetMembership(elem: Expr, set: ArrayExpr): BoolExpr

  /**
    * Check for subsetness of sets.
    **/
  def mkSetSubset(arg1: ArrayExpr, arg2: ArrayExpr): BoolExpr

  /**
    * Create the empty sequence.
    */
  def MkEmptySeq(s: Sort): SeqExpr

  /**
    * Create the singleton sequence.
    */
  def MkUnit(elem: Expr): SeqExpr

  /**
    * Create a string constant.
    */
  def MkString(s: String): SeqExpr

  /**
    * Concatentate sequences.
    */
  def MkConcat(t: SeqExpr*): SeqExpr

  /**
    * Retrieve the length of a given sequence.
    */
  def MkLength(s: SeqExpr): IntExpr

  /**
    * Check for sequence prefix.
    */
  def MkPrefixOf(s1: SeqExpr, s2: SeqExpr): BoolExpr

  /**
    * Check for sequence suffix.
    */
  def MkSuffixOf(s1: SeqExpr, s2: SeqExpr): BoolExpr

  /**
    * Check for sequence containment of s2 in s1.
    */
  def MkContains(s1: SeqExpr, s2: SeqExpr): BoolExpr

  /**
    * Retrieve sequence of length one at index.
    */
  def MkAt(s: SeqExpr, index: IntExpr): SeqExpr

  /**
    * Extract subsequence.
    */
  def MkExtract(s: SeqExpr, offset: IntExpr, length: IntExpr): SeqExpr

  /**
    * Extract index of sub-string starting at offset.
    */
  def MkIndexOf(s: SeqExpr, substr: SeqExpr, offset: ArithExpr): IntExpr

  /**
    * Replace the first occurrence of src by dst in s.
    */
  def MkReplace(s: SeqExpr, src: SeqExpr, dst: SeqExpr): SeqExpr

  /**
    * Convert a regular expression that accepts sequence s.
    */
  def MkToRe(s: SeqExpr): ReExpr

  /**
    * Check for regular expression membership.
    */
  def MkInRe(s: SeqExpr, re: ReExpr): BoolExpr

  /**
    * Take the Kleene star of a regular expression.
    */
  def MkStar(re: ReExpr): ReExpr

  /**
    * Take the Kleene plus of a regular expression.
    */
  def MPlus(re: ReExpr): ReExpr

  /**
    * Create the optional regular expression.
    */
  def MOption(re: ReExpr): ReExpr

  /**
    * Create the concatenation of regular languages.
    */
  def MkConcatRE(t: ReExpr*): ReExpr

  /**
    * Create the union of regular languages.
    */
  def MkUnion(t: ReExpr*): ReExpr

  /**
    * Create a Term of a given sort.
    *
    * @param v  A string representing the term value in decimal notation. If the given sort is a real, then the
    *           Term can be a rational, that is, a string of the form
    *           { @code [num]* / [num]*}.
    * @param ty The sort of the
    * numeral. In the current implementation, the given sort can be an int,
    *           real, or bit-vectors of arbitrary size.
    * @return A Term with value { @code v} and sort { @code ty}
    **/
  def mkNumeral(v: String, ty: Sort): Expr

  /**
    * Create a Term of a given sort. This function can be use to create
    * numerals that fit in a machine integer. It is slightly faster than
    * {@code MakeNumeral} since it is not necessary to parse a string.
    *
    * @param v  Value of the numeral
    * @param ty Sort of the numeral
    * @return A Term with value { @code v} and type { @code ty}
    **/
  def mkNumeral(v: Int, ty: Sort): Expr

  def mkNumeral(v: Long, ty: Sort): Expr

  /**
    * Create a real from a fraction.
    *
    * @param num numerator of rational.
    * @param den denominator of rational.
    * @return A Term with value { @code num}/{ @code den}
    *         and sort Real
    * @see #mkNumeral(String,Sort)
    **/
  def mkReal(num: Int, den: Int): RatNum

  /**
    * Create a real numeral.
    *
    * @param v A string representing the Term value in decimal notation.
    * @return A Term with value { @code v} and sort Real
    **/
  def mkReal(v: String): RatNum

  /**
    * Create a real numeral.
    *
    * @param v value of the numeral.
    * @return A Term with value { @code v} and sort Real
    **/
  def mkReal(v: Int): RatNum

  def mkReal(v: Long): RatNum

  /**
    * Create an integer numeral.
    *
    * @param v A string representing the Term value in decimal notation.
    **/
  def mkInt(v: String): IntNum

  /**
    * Create an integer numeral.
    *
    * @param v value of the numeral.
    * @return A Term with value { @code v} and sort Integer
    **/
  def mkInt(v: Int): IntNum

  def mkInt(v: Long): IntNum

  /**
    * Create a bit-vector numeral.
    *
    * @param v    A string representing the value in decimal notation.
    * @param size the size of the bit-vector
    **/
  def mkBV(v: String, size: Int): BitVecNum

  /**
    * Create a bit-vector numeral.
    *
    * @param v    value of the numeral.
    * @param size the size of the bit-vector
    **/
  def mkBV(v: Int, size: Int): BitVecNum

  /**
    * Create a bit-vector numeral.
    *
    * @param v    value of the numeral. *
    * @param size the size of the bit-vector
    **/
  def mkBV(v: Long, size: Int): BitVecNum

  /**
    * Create a universal Quantifier.
    *
    * @param sorts        the sorts of the bound variables.
    * @param names        names of the bound variables
    * @param body         the body of the quantifier.
    * @param weight       quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
    * @param patterns     array containing the patterns created using { @code MkPattern}.
    * @param noPatterns   array containing the anti-patterns created using { @code MkPattern}.
    * @param quantifierID optional symbol to track quantifier.
    * @param skolemID     optional symbol to track skolem constants.
    * @return Creates a forall formula, where
    *         { @code weight} is the weight, { @code patterns} is
    *         an array of patterns, { @code sorts} is an array with the sorts
    *         of the bound variables, { @code names} is an array with the
    *         'names' of the bound variables, and { @code body} is the body
    *         of the quantifier. Quantifiers are associated with weights indicating the
    *         importance of using the quantifier during instantiation.
    *         Note that the bound variables are de-Bruijn indices created using { @link #mkBound}.
    *         Z3 applies the convention that the last element in { @code names} and
    *         { @code sorts} refers to the variable with index 0, the second to last element
    *         of { @code names} and { @code sorts} refers to the variable
    *         with index 1, etc.
    **/
  def mkForall(sorts: Array[Sort], names: Array[Symbol], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier

  /**
    * Creates a universal quantifier using a list of constants that will form the set of bound variables.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  def mkForall(boundConstants: Array[Expr], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier

  /**
    * Creates an existential quantifier using de-Brujin indexed variables.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  def mkExists(sorts: Array[Sort], names: Array[Symbol], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier

  /**
    * Creates an existential quantifier using a list of constants that will form the set of bound variables.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  def mkExists(boundConstants: Array[Expr], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier

  /**
    * Create a Quantifier.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  def mkQuantifier(universal: Boolean, sorts: Array[Sort], names: Array[Symbol], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier

  /**
    * Create a Quantifier
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  def mkQuantifier(universal: Boolean, boundConstants: Array[Expr], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier

  /**
    * Selects the format used for pretty-printing expressions.
    * Remarks:  The
    * default mode for pretty printing expressions is to produce SMT-LIB style
    * output where common subexpressions are printed at each occurrence. The
    * mode is called Z3_PRINT_SMTLIB_FULL. To print shared common
    * subexpressions only once, use the Z3_PRINT_LOW_LEVEL mode. To print in
    * way that conforms to SMT-LIB standards and uses let expressions to share
    * common sub-expressions use Z3_PRINT_SMTLIB_COMPLIANT.
    *
    * @see AST#toString
    * @see Pattern#toString
    * @see FuncDecl#toString
    * @see Sort#toString
    **/
  def setPrintMode(value: Z3_ast_print_mode): Unit

  /**
    * Convert a benchmark into an SMT-LIB formatted string.
    *
    * @param name        Name of the benchmark. The argument is optional.
    * @param logic       The benchmark logic.
    * @param status      The status string (sat, unsat, or unknown)
    * @param attributes  Other attributes, such as source, difficulty or
    *                    category.
    * @param assumptions Auxiliary assumptions.
    * @param formula     Formula to be checked for consistency in conjunction with assumptions.
    * @return A string representation of the benchmark.
    **/
  def benchmarkToSMTString(name: String, logic: String, status: String, attributes: String, assumptions: Array[BoolExpr], formula: BoolExpr): String

  /**
    * Parse the given string using the SMT-LIB parser.
    * Remarks:  The symbol
    * table of the parser can be initialized using the given sorts and
    * declarations. The symbols in the arrays {@code sortNames} and
    * {@code declNames} don't need to match the names of the sorts
    * and declarations in the arrays {@code sorts} and {@code decls}. This is a useful feature since we can use arbitrary names
    * to reference sorts and declarations.
    **/
  def parseSMTLIBString(str: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Unit

  /**
    * Parse the given file using the SMT-LIB parser.
    *
    * @see #parseSMTLIBString
    **/
  def parseSMTLIBFile(fileName: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Unit


  /**
    * Parse the given string using the SMT-LIB2 parser.
    *
    * @see #parseSMTLIBString
    * @return A conjunction of assertions in the scope (up to push/pop) at the
    *         end of the string.
    **/
  def parseSMTLIB2String(str: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Array[BoolExpr]

  /**
    * Parse the given file using the SMT-LIB2 parser.
    *
    * @see #parseSMTLIB2String
    **/
  def parseSMTLIB2File(fileName: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Array[BoolExpr]

  /**
    * Creates a new Goal.
    * Remarks:  Note that the Context must have been
    * created with proof generation support if {@code proofs} is set
    * to true here.
    *
    * @param models     Indicates whether model generation should be enabled.
    * @param unsatCores Indicates whether unsat core generation should be enabled.
    * @param proofs     Indicates whether proof generation should be
    *                   enabled.
    **/
  def mkGoal(models: Boolean, unsatCores: Boolean, proofs: Boolean): Goal

  /**
    * Creates a new ParameterSet.
    **/
  def mkParams: Params

  /**
    * The number of supported tactics.
    **/
  def getNumTactics: Int

  /**
    * The names of all supported tactics.
    **/
  def getTacticNames: Array[String]

  /**
    * Returns a string containing a description of the tactic with the given
    * name.
    **/
  def getTacticDescription(name: String): String

  /**
    * Creates a new Tactic.
    **/
  def mkTactic(name: String): Tactic

  /**
    * Create a tactic that applies {@code t1} to a Goal and then
    * {@code t2"/> to every subgoal produced by <paramref name="t1}.
    **/
  def andThen(t1: Tactic, t2: Tactic, ts: Tactic*): Tactic

  /**
    * Create a tactic that applies {@code t1} to a Goal and then
    * {@code t2} to every subgoal produced by {@code t1}
    *
    * Remarks:  Shorthand for {@code AndThen}.
    **/
  def then(t1: Tactic, t2: Tactic, ts: Tactic*): Tactic

  /**
    * Create a tactic that first applies {@code t1} to a Goal and if
    * it fails then returns the result of {@code t2} applied to the
    * Goal.
    **/
  def orElse(t1: Tactic, t2: Tactic): Tactic

  /**
    * Create a tactic that applies {@code t} to a goal for {@code ms} milliseconds.
    * Remarks:  If {@code t} does not
    * terminate within {@code ms} milliseconds, then it fails.
    *
    **/
  def tryFor(t: Tactic, ms: Int): Tactic

  /**
    * Create a tactic that applies {@code t} to a given goal if the
    * probe {@code p} evaluates to true.
    * Remarks:  If {@code p} evaluates to false, then the new tactic behaves like the
    * {@code skip} tactic.
    **/
  def when(p: Probe, t: Tactic): Tactic

  /**
    * Create a tactic that applies {@code t1} to a given goal if the
    * probe {@code p"/> evaluates to true and <paramref name="t2}
    * otherwise.
    **/
  def cond(p: Probe, t1: Tactic, t2: Tactic): Tactic

  /**
    * Create a tactic that keeps applying {@code t} until the goal
    * is not modified anymore or the maximum number of iterations {@code max} is reached.
    **/
  def repeat(t: Tactic, max: Int): Tactic

  /**
    * Create a tactic that just returns the given goal.
    **/
  def skip: Tactic

  /**
    * Create a tactic always fails.
    **/
  def fail: Tactic

  /**
    * Create a tactic that fails if the probe {@code p} evaluates to
    * false.
    **/
  def failIf(p: Probe): Tactic

  /**
    * Create a tactic that fails if the goal is not triviall satisfiable (i.e.,
    * empty) or trivially unsatisfiable (i.e., contains `false').
    **/
  def failIfNotDecided: Tactic

  /**
    * Create a tactic that applies {@code t} using the given set of
    * parameters {@code p}.
    **/
  def usingParams(t: Tactic, p: Params): Tactic

  /**
    * Create a tactic that applies {@code t} using the given set of
    * parameters {@code p}.
    * Remarks: Alias for
    * {@code UsingParams}
    **/
  def `with`(t: Tactic, p: Params): Tactic

  /**
    * Create a tactic that applies the given tactics in parallel until one of them succeeds (i.e., the first that doesn't fail).
    **/
  def parOr(t: Tactic*): Tactic

  /**
    * Create a tactic that applies {@code t1} to a given goal and
    * then {@code t2} to every subgoal produced by {@code t1}. The subgoals are processed in parallel.
    **/
  def parAndThen(t1: Tactic, t2: Tactic): Tactic

  /**
    * Interrupt the execution of a Z3 procedure.
    * Remarks: This procedure can be
    * used to interrupt: solvers, simplifiers and tactics.
    **/
  def interrupt(): Unit

  /**
    * The number of supported Probes.
    **/
  def getNumProbes: Int

  /**
    * The names of all supported Probes.
    **/
  def getProbeNames: Array[String]

  /**
    * Returns a string containing a description of the probe with the given
    * name.
    **/
  def getProbeDescription(name: String): String

  /**
    * Creates a new Probe.
    **/
  def mkProbe(name: String): Probe

  /**
    * Create a probe that always evaluates to {@code val}.
    **/
  def constProbe(`val`: Double): Probe

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is less than the value returned by {@code p2}
    **/
  def lt(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is greater than the value returned by {@code p2}
    **/
  def gt(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is less than or equal the value returned by
    * {@code p2}
    **/
  def le(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is greater than or equal the value returned by
    * {@code p2}
    **/
  def ge(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is equal to the value returned by {@code p2}
    **/
  def eq(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value {@code p1} and {@code p2} evaluate to "true".
    **/
  def and(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value {@code p1} or {@code p2} evaluate to "true".
    **/
  def or(p1: Probe, p2: Probe): Probe

  /**
    * Create a probe that evaluates to "true" when the value {@code p} does not evaluate to "true".
    **/
  def not(p: Probe): Probe

  /**
    * Creates a new (incremental) solver.
    * Remarks:  This solver also uses a set
    * of builtin tactics for handling the first check-sat command, and
    * check-sat commands that take more than a given number of milliseconds to
    * be solved.
    **/
  def mkSolver(): Solver

  def mkSolver(logic: Symbol): Solver

  /**
    * Creates a new (incremental) solver.
    *
    * @see #mkSolver(Symbol)
    **/
  def mkSolver(logic: String): Solver

  /**
    * Creates a new (incremental) solver.
    **/
  def mkSimpleSolver: Solver

  /**
    * Creates a solver that is implemented using the given tactic.
    * Remarks:
    * The solver supports the commands {@code Push} and {@code Pop},
    * but it will always solve each check from scratch.
    **/
  def mkSolver(t: Tactic): Solver

  /**
    * Create a Fixedpoint context.
    **/
  def mkFixedpoint: Fixedpoint

  /**
    * Create a Optimize context.
    **/
  def mkOptimize: Optimize

  /**
    * Create the floating-point RoundingMode sort.
    *
    * @throws Z3Exception
    **/
  def mkFPRoundingModeSort: FPRMSort

  /**
    * Create a numeral of RoundingMode sort which represents the NearestTiesToEven rounding mode.
    *
    * @throws Z3Exception
    **/
  def mkFPRoundNearestTiesToEven: FPRMExpr

  def mkFPRNE: FPRMNum

  /**
    * Create a numeral of RoundingMode sort which represents the NearestTiesToAway rounding mode.
    *
    * @throws Z3Exception
    **/
  def mkFPRoundNearestTiesToAway: FPRMNum

  def mkFPRNA: FPRMNum

  /**
    * Create a numeral of RoundingMode sort which represents the RoundTowardPositive rounding mode.
    *
    * @throws Z3Exception
    **/
  def mkFPRoundTowardPositive: FPRMNum

  def mkFPRTP: FPRMNum

  /**
    * Create a numeral of RoundingMode sort which represents the RoundTowardNegative rounding mode.
    *
    * @throws Z3Exception
    **/
  def mkFPRoundTowardNegative: FPRMNum

  def mkFPRTN: FPRMNum

  /**
    * Create a numeral of RoundingMode sort which represents the RoundTowardZero rounding mode.
    *
    * @throws Z3Exception
    **/
  def mkFPRoundTowardZero: FPRMNum

  def mkFPRTZ: FPRMNum

  /**
    * Create a FloatingPoint sort.
    *
    * @param ebits exponent bits in the FloatingPoint sort.
    * @param sbits significand bits in the FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFPSort(ebits: Int, sbits: Int): FPSort

  /**
    * Create the half-precision (16-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  def mkFPSortHalf: FPSort

  def mkFPSort16: FPSort

  /**
    * Create the single-precision (32-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  def mkFPSortSingle: FPSort

  def mkFPSort32: FPSort

  /**
    * Create the double-precision (64-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  def mkFPSortDouble: FPSort

  def mkFPSort64: FPSort

  /**
    * Create the quadruple-precision (128-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  def mkFPSortQuadruple: FPSort

  def mkFPSort128: FPSort

  /**
    * Create a NaN of sort s.
    *
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFPNaN(s: FPSort): FPNum

  /**
    * Create a floating-point infinity of sort s.
    *
    * @param s        FloatingPoint sort.
    * @param negative indicates whether the result should be negative.
    * @throws Z3Exception
    **/
  def mkFPInf(s: FPSort, negative: Boolean): FPNum

  /**
    * Create a floating-point zero of sort s.
    *
    * @param s        FloatingPoint sort.
    * @param negative indicates whether the result should be negative.
    * @throws Z3Exception
    **/
  def mkFPZero(s: FPSort, negative: Boolean): FPNum

  /**
    * Create a numeral of FloatingPoint sort from a float.
    *
    * @param v numeral value.
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFPNumeral(v: Float, s: FPSort): FPNum

  def mkFPNumeral(v: Double, s: FPSort): FPNum

  /**
    * Create a numeral of FloatingPoint sort from an int.
    * * @param v numeral value.
    *
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFPNumeral(v: Int, s: FPSort): FPNum

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two integers.
    *
    * @param sgn the sign.
    * @param sig the significand.
    * @param exp the exponent.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFPNumeral(sgn: Boolean, exp: Int, sig: Int, s: FPSort): FPNum

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two 64-bit integers.
    *
    * @param sgn the sign.
    * @param sig the significand.
    * @param exp the exponent.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFPNumeral(sgn: Boolean, exp: Long, sig: Long, s: FPSort): FPNum

  def mkFP(v: Float, s: FPSort): FPNum

  def mkFP(v: Double, s: FPSort): FPNum

  /**
    * Create a numeral of FloatingPoint sort from an int.
    *
    * @param v numeral value.
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFP(v: Int, s: FPSort): FPNum

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two integers.
    *
    * @param sgn the sign.
    * @param exp the exponent.
    * @param sig the significand.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFP(sgn: Boolean, exp: Int, sig: Int, s: FPSort): FPNum

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two 64-bit integers.
    *
    * @param sgn the sign.
    * @param exp the exponent.
    * @param sig the significand.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  def mkFP(sgn: Boolean, exp: Long, sig: Long, s: FPSort): FPNum

  /**
    * Floating-point absolute value
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPAbs(t: FPExpr): FPExpr

  /**
    * Floating-point negation
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPNeg(t: FPExpr): FPExpr

  /**
    * Floating-point addition
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPAdd(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Floating-point subtraction
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPSub(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Floating-point multiplication
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPMul(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Floating-point division
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPDiv(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Floating-point fused multiply-add
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @param t3 floating-point term
    *           Remarks:
    *           The result is round((t1 * t2) + t3)
    * @throws Z3Exception
    **/
  def mkFPFMA(rm: FPRMExpr, t1: FPExpr, t2: FPExpr, t3: FPExpr): FPExpr

  /**
    * Floating-point square root
    *
    * @param rm rounding mode term
    * @param t  floating-point term
    * @throws Z3Exception
    **/
  def mkFPSqrt(rm: FPRMExpr, t: FPExpr): FPExpr

  /**
    * Floating-point remainder
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPRem(t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Floating-point roundToIntegral. Rounds a floating-point number to
    * the closest integer, again represented as a floating-point number.
    *
    * @param rm term of RoundingMode sort
    * @param t  floating-point term
    * @throws Z3Exception
    **/
  def mkFPRoundToIntegral(rm: FPRMExpr, t: FPExpr): FPExpr

  /**
    * Minimum of floating-point numbers.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPMin(t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Maximum of floating-point numbers.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPMax(t1: FPExpr, t2: FPExpr): FPExpr

  /**
    * Floating-point less than or equal.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPLEq(t1: FPExpr, t2: FPExpr): BoolExpr

  /**
    * Floating-point less than.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPLt(t1: FPExpr, t2: FPExpr): BoolExpr

  /**
    * Floating-point greater than or equal.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPGEq(t1: FPExpr, t2: FPExpr): BoolExpr

  /**
    * Floating-point greater than.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  def mkFPGt(t1: FPExpr, t2: FPExpr): BoolExpr

  /**
    * Floating-point equality.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    *           Remarks:
    *           Note that this is IEEE 754 equality (as opposed to standard =).
    * @throws Z3Exception
    **/
  def mkFPEq(t1: FPExpr, t2: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a normal floating-point number.\
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsNormal(t: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a subnormal floating-point number.\
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsSubnormal(t: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a floating-point number with zero value, i.e., +0 or -0.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsZero(t: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a floating-point number representing +oo or -oo.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsInfinite(t: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a NaN.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsNaN(t: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a negative floating-point number.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsNegative(t: FPExpr): BoolExpr

  /**
    * Predicate indicating whether t is a positive floating-point number.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  def mkFPIsPositive(t: FPExpr): BoolExpr

  /**
    * Create an expression of FloatingPoint sort from three bit-vector expressions.
    *
    * @param sgn bit-vector term (of size 1) representing the sign.
    * @param sig bit-vector term representing the significand.
    * @param exp bit-vector term representing the exponent.
    *            Remarks:
    *            This is the operator named `fp' in the SMT FP theory definition.
    *            Note that sgn is required to be a bit-vector of size 1. Significand and exponent
    *            are required to be greater than 1 and 2 respectively. The FloatingPoint sort
    *            of the resulting expression is automatically determined from the bit-vector sizes
    *            of the arguments.
    *            @throws Z3Exception
    **/
  def mkFP(sgn: BitVecExpr, sig: BitVecExpr, exp: BitVecExpr): FPExpr

  /**
    * Conversion of a single IEEE 754-2008 bit-vector into a floating-point number.
    *
    * @param bv bit-vector value (of size m).
    * @param s  FloatingPoint sort (ebits+sbits == m)
    *           Remarks:
    *           Produces a term that represents the conversion of a bit-vector term bv to a
    *           floating-point term of sort s. The bit-vector size of bv (m) must be equal
    *           to ebits+sbits of s. The format of the bit-vector is as defined by the
    *           IEEE 754-2008 interchange format.
    * @throws Z3Exception
    **/
  def mkFPToFP(bv: BitVecExpr, s: FPSort): FPExpr

  /**
    * Conversion of a FloatingPoint term into another term of different FloatingPoint sort.
    *
    * @param rm RoundingMode term.
    * @param t  FloatingPoint term.
    * @param s  FloatingPoint sort.
    *           Remarks:
    *           Produces a term that represents the conversion of a floating-point term t to a
    *           floating-point term of sort s. If necessary, the result will be rounded according
    *           to rounding mode rm.
    * @throws Z3Exception
    **/
  def mkFPToFP(rm: FPRMExpr, t: FPExpr, s: FPSort): FPExpr

  /**
    * Conversion of a term of real sort into a term of FloatingPoint sort.
    *
    * @param rm RoundingMode term.
    * @param t  term of Real sort.
    * @param s  FloatingPoint sort.
    *           Remarks:
    *           Produces a term that represents the conversion of term t of real sort into a
    *           floating-point term of sort s. If necessary, the result will be rounded according
    *           to rounding mode rm.
    * @throws Z3Exception
    **/
  def mkFPToFP(rm: FPRMExpr, t: RealExpr, s: FPSort): FPExpr

  /**
    * Conversion of a 2's complement signed bit-vector term into a term of FloatingPoint sort.
    *
    * @param rm     RoundingMode term.
    * @param t      term of bit-vector sort.
    * @param s      FloatingPoint sort.
    * @param signed flag indicating whether t is interpreted as signed or unsigned bit-vector.
    *               Remarks:
    *               Produces a term that represents the conversion of the bit-vector term t into a
    *               floating-point term of sort s. The bit-vector t is taken to be in signed
    *               2's complement format (when signed==true, otherwise unsigned). If necessary, the
    *               result will be rounded according to rounding mode rm.
    * @throws Z3Exception
    **/
  def mkFPToFP(rm: FPRMExpr, t: BitVecExpr, s: FPSort, signed: Boolean): FPExpr

  /**
    * Conversion of a floating-point number to another FloatingPoint sort s.
    *
    * @param s  FloatingPoint sort
    * @param rm floating-point rounding mode term
    * @param t  floating-point term
    *           Remarks:
    *           Produces a term that represents the conversion of a floating-point term t to a different
    *           FloatingPoint sort s. If necessary, rounding according to rm is applied.
    * @throws Z3Exception
    **/
  def mkFPToFP(s: FPSort, rm: FPRMExpr, t: FPExpr): FPExpr

  /**
    * Conversion of a floating-point term into a bit-vector.
    *
    * @param rm     RoundingMode term.
    * @param t      FloatingPoint term
    * @param sz     Size of the resulting bit-vector.
    * @param signed Indicates whether the result is a signed or unsigned bit-vector.
    *               Remarks:
    *               Produces a term that represents the conversion of the floating-poiunt term t into a
    *               bit-vector term of size sz in 2's complement format (signed when signed==true). If necessary,
    *               the result will be rounded according to rounding mode rm.
    * @throws Z3Exception
    **/
  def mkFPToBV(rm: FPRMExpr, t: FPExpr, sz: Int, signed: Boolean): BitVecExpr

  /**
    * Conversion of a floating-point term into a real-numbered term.
    *
    * @param t FloatingPoint term
    *          Remarks:
    *          Produces a term that represents the conversion of the floating-poiunt term t into a
    *          real number. Note that this type of conversion will often result in non-linear
    *          constraints over real terms.
    * @throws Z3Exception
    **/
  def mkFPToReal(t: FPExpr): RealExpr

  /**
    * Conversion of a floating-point term into a bit-vector term in IEEE 754-2008 format.
    *
    * @param t FloatingPoint term.
    *          Remarks:
    *          The size of the resulting bit-vector is automatically determined. Note that
    *          IEEE 754-2008 allows multiple different representations of NaN. This conversion
    *          knows only one NaN and it will always produce the same bit-vector represenatation of
    *          that NaN.
    * @throws Z3Exception
    **/
  def mkFPToIEEEBV(t: FPExpr): BitVecExpr

  /**
    * Conversion of a real-sorted significand and an integer-sorted exponent into a term of FloatingPoint sort.
    *
    * @param rm  RoundingMode term.
    * @param exp Exponent term of Int sort.
    * @param sig Significand term of Real sort.
    * @param s   FloatingPoint sort.
    *            Remarks:
    *            Produces a term that represents the conversion of sig * 2^exp into a
    *            floating-point term of sort s. If necessary, the result will be rounded
    *            according to rounding mode rm.
    * @throws Z3Exception
    **/
  def mkFPToFP(rm: FPRMExpr, exp: IntExpr, sig: RealExpr, s: FPSort): BitVecExpr

  /**
    * Wraps an AST.
    * Remarks: This function is used for transitions between
    * native and managed objects. Note that {@code nativeObject}
    * must be a native object obtained from Z3 (e.g., through
    * {@code UnwrapAST}) and that it must have a correct reference count.
    *
    * @see Native#incRef
    * @see #unwrapAST
    * @param nativeObject The native pointer to wrap.
    **/
  def wrapAST(nativeObject: Long): AST

  /**
    * Unwraps an AST.
    * Remarks: This function is used for transitions between
    * native and managed objects. It returns the native pointer to the AST.
    * Note that AST objects are reference counted and unwrapping an AST
    * disables automatic reference counting, i.e., all references to the IntPtr
    * that is returned must be handled externally and through native calls (see
    * e.g.,
    *
    * @see Native#incRef
    * @see #wrapAST
    * @param a The AST to unwrap.
    **/
  def unwrapAST(a: AST): Long

  /**
    * Return a string describing all available parameters to
    * {@code Expr.Simplify}.
    **/
  def SimplifyHelp: String

  /**
    * Retrieves parameter descriptions for simplifier.
    **/
  def getSimplifyParameterDescriptions: ParamDescrs

  /**
    * Update a mutable configuration parameter.
    * Remarks:  The list of all
    * configuration parameters can be obtained using the Z3 executable:
    * {@code z3.exe -ini?} Only a few configuration parameters are mutable
    * once the context is created. An exception is thrown when trying to modify
    * an immutable parameter.
    **/
  def updateParamValue(id: String, value: String): Unit

  def getConstructorDRQ: IDecRefQueue[Constructor]

  def getConstructorListDRQ: IDecRefQueue[ConstructorList]

  def getASTDRQ: IDecRefQueue[AST]

  def getASTMapDRQ: IDecRefQueue[_ <: Z3Object]

  def getASTVectorDRQ: IDecRefQueue[ASTVector]

  def getApplyResultDRQ: IDecRefQueue[ApplyResult]

  def getFuncEntryDRQ: IDecRefQueue[FuncInterp.Entry]

  def getFuncInterpDRQ: IDecRefQueue[FuncInterp]

  def getGoalDRQ: IDecRefQueue[Goal]

  def getModelDRQ: IDecRefQueue[Model]

  def getParamsDRQ: IDecRefQueue[Params]

  def getParamDescrsDRQ: IDecRefQueue[ParamDescrs]

  def getProbeDRQ: IDecRefQueue[Probe]

  def getSolverDRQ: IDecRefQueue[Solver]

  def getStatisticsDRQ: IDecRefQueue[Statistics]

  def getTacticDRQ: IDecRefQueue[Tactic]

  def getFixedpointDRQ: IDecRefQueue[Fixedpoint]

  def getOptimizeDRQ: IDecRefQueue[Optimize]

  /**
    * Disposes of the context.
    **/
  override def close(): Unit
}