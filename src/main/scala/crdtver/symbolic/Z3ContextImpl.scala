package crdtver.symbolic

import com.microsoft.z3
import com.microsoft.z3._
import com.microsoft.z3.Quantifier
import com.microsoft.z3.enumerations.Z3_ast_print_mode

//noinspection ScalaDocInlinedTag,ScalaDocParserErrorInspection
class Z3ContextImpl(val context: Context) extends Z3Context {
  /**
    * Creates a new symbol using an integer.
    * Remarks: Not all integers can be passed to this function.
    * The legal range of unsigned integers is 0 to 2^30-1.
    **/
  override def mkSymbol(i: Int): IntSymbol = context.mkSymbol(i)

  /**
    * Create a symbol using a string.
    **/
  override def mkSymbol(name: String): StringSymbol = context.mkSymbol(name)

  /**
    * Retrieves the Boolean sort of the context.
    **/
  //    /**
  //     * Create an array of symbols.
  //     **/
  //    Symbol[] mkSymbols(String[] names)
  //    {
  //        return context.mkSymbols(names);
  //    }
  override def getBoolSort: BoolSort = context.getBoolSort

  /**
    * Retrieves the Integer sort of the context.
    **/
  override def getIntSort: IntSort = context.getIntSort

  /**
    * Retrieves the Real sort of the context.
    **/
  override def getRealSort: RealSort = context.getRealSort

  /**
    * Create a new Boolean sort.
    **/
  override def mkBoolSort: BoolSort = context.mkBoolSort

  override def getStringSort: SeqSort = context.getStringSort

  /**
    * Create a new uninterpreted sort.
    **/
  override def mkUninterpretedSort(s: Symbol): UninterpretedSort = context.mkUninterpretedSort(s)

  override def mkUninterpretedSort(str: String): UninterpretedSort = context.mkUninterpretedSort(str)

  /**
    * Create a new integer sort.
    **/
  override def mkIntSort: IntSort = context.mkIntSort

  /**
    * Create a real sort.
    **/
  override def mkRealSort: RealSort = context.mkRealSort

  /**
    * Create a new bit-vector sort.
    **/
  override def mkBitVecSort(size: Int): BitVecSort = context.mkBitVecSort(size)

  /**
    * Create a new array sort.
    **/
  override def mkArraySort(domain: Sort, range: Sort): ArraySort = context.mkArraySort(domain, range)

  /**
    * Create a new string sort
    **/
  override def mkStringSort: SeqSort = context.mkStringSort

  /**
    * Create a new sequence sort
    **/
  override def mkSeqSort(s: Sort): SeqSort = context.mkSeqSort(s)

  /**
    * Create a new regular expression sort
    **/
  override def mkReSort(s: Sort): ReSort = context.mkReSort(s)

  /**
    * Create a new tuple sort.
    **/
  override def mkTupleSort(name: Symbol, fieldNames: Array[Symbol], fieldSorts: Array[Sort]): TupleSort = context.mkTupleSort(name, fieldNames, fieldSorts)

  /**
    * Create a new enumeration sort.
    **/
  override def mkEnumSort(name: Symbol, enumNames: Symbol*): EnumSort = context.mkEnumSort(name, enumNames:_*)

  override def mkEnumSort(name: String, enumNames: String*): EnumSort = context.mkEnumSort(name, enumNames:_*)

  /**
    * Create a new list sort.
    **/
  override def mkListSort(name: Symbol, elemSort: Sort): ListSort = context.mkListSort(name, elemSort)

  override def mkListSort(name: String, elemSort: Sort): ListSort = context.mkListSort(name, elemSort)

  /**
    * Create a new finite domain sort.
    **/
  override def mkFiniteDomainSort(name: Symbol, size: Long): FiniteDomainSort = context.mkFiniteDomainSort(name, size)

  override def mkFiniteDomainSort(name: String, size: Long): FiniteDomainSort = context.mkFiniteDomainSort(name, size)

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
  override def mkConstructor(name: Symbol, recognizer: Symbol, fieldNames: Array[Symbol], sorts: Array[Sort], sortRefs: Array[Int]): Constructor = context.mkConstructor(name, recognizer, fieldNames, sorts, sortRefs)

  /**
    * Create a datatype constructor.
    **/
  override def mkConstructor(name: String, recognizer: String, fieldNames: Array[String], sorts: Array[Sort], sortRefs: Array[Int]): Constructor = context.mkConstructor(name, recognizer, fieldNames, sorts, sortRefs)

  /**
    * Create a new datatype sort.
    **/
  override def mkDatatypeSort(name: Symbol, constructors: Array[Constructor]): DatatypeSort = context.mkDatatypeSort(name, constructors)

  override def mkDatatypeSort(name: String, constructors: Array[Constructor]): DatatypeSort = context.mkDatatypeSort(name, constructors)

  /**
    * Create mutually recursive datatypes.
    *
    * @param names names of datatype sorts
    * @param c     list of constructors, one list per sort.
    **/
  override def mkDatatypeSorts(names: Array[Symbol], c: Array[Array[Constructor]]): Array[DatatypeSort] = context.mkDatatypeSorts(names, c)

  /**
    * Create mutually recursive data-types.
    **/
  override def mkDatatypeSorts(names: Array[String], c: Array[Array[Constructor]]): Array[DatatypeSort] = context.mkDatatypeSorts(names, c)

  /**
    * Update a datatype field at expression t with value v.
    * The function performs a record update at t. The field
    * that is passed in as argument is updated with value v,
    * the remaining fields of t are unchanged.
    **/
  @throws[Z3Exception]
  override def MkUpdateField(field: FuncDecl, t: Expr, v: Expr): Expr = context.mkUpdateField(field, t, v)

  /**
    * Creates a new function declaration.
    **/
  override def mkFuncDecl(name: Symbol, domain: Array[Sort], range: Sort): FuncDecl = context.mkFuncDecl(name, domain, range)

  override def mkFuncDecl(name: Symbol, domain: Sort, range: Sort): FuncDecl = context.mkFuncDecl(name, domain, range)

  override def mkFuncDecl(name: String, domain: Array[Sort], range: Sort): FuncDecl = context.mkFuncDecl(name, domain, range)

  override def mkFuncDecl(name: String, domain: Sort, range: Sort): FuncDecl = context.mkFuncDecl(name, domain, range)

  /**
    * Creates a fresh function declaration with a name prefixed with
    * {@code prefix}.
    *
    * @see #mkFuncDecl(String,Sort,Sort)
    * @see #mkFuncDecl(String,Sort[],Sort)
    **/
  override def mkFreshFuncDecl(prefix: String, domain: Array[Sort], range: Sort): FuncDecl = context.mkFreshFuncDecl(prefix, domain, range)

  /**
    * Creates a new constant function declaration.
    **/
  override def mkConstDecl(name: Symbol, range: Sort): FuncDecl = context.mkConstDecl(name, range)

  override def mkConstDecl(name: String, range: Sort): FuncDecl = context.mkConstDecl(name, range)

  /**
    * Creates a fresh constant function declaration with a name prefixed with
    * {@code prefix"}.
    *
    * @see #mkFuncDecl(String,Sort,Sort)
    * @see #mkFuncDecl(String,Sort[],Sort)
    **/
  override def mkFreshConstDecl(prefix: String, range: Sort): FuncDecl = context.mkFreshConstDecl(prefix, range)

  /**
    * Creates a new bound variable.
    *
    * @param index The de-Bruijn index of the variable
    * @param ty    The sort of the variable
    **/
  override def mkBound(index: Int, ty: Sort): Expr = context.mkBound(index, ty)

  /**
    * Create a quantifier pattern.
    **/
  override def mkPattern(terms: Expr*): Pattern = context.mkPattern(terms:_*)

  /**
    * Creates a new Constant of sort {@code range} and named
    * {@code name}.
    **/
  override def mkConst(name: Symbol, range: Sort): Expr = context.mkConst(name, range)

  override def mkConst(name: String, range: Sort): Expr = context.mkConst(name, range)

  /**
    * Creates a fresh Constant of sort {@code range} and a name
    * prefixed with {@code prefix}.
    **/
  override def mkFreshConst(prefix: String, range: Sort): Expr = context.mkFreshConst(prefix, range)

  /**
    * Creates a fresh constant from the FuncDecl {@code f}.
    *
    * @param f A decl of a 0-arity function
    **/
  override def mkConst(f: FuncDecl): Expr = context.mkConst(f)

  /**
    * Create a Boolean constant.
    **/
  override def mkBoolConst(name: Symbol): BoolExpr = context.mkBoolConst(name)

  override def mkBoolConst(name: String): BoolExpr = context.mkBoolConst(name)

  /**
    * Creates an integer constant.
    **/
  override def mkIntConst(name: Symbol): IntExpr = context.mkIntConst(name)

  override def mkIntConst(name: String): IntExpr = context.mkIntConst(name)

  /**
    * Creates a real constant.
    **/
  override def mkRealConst(name: Symbol): RealExpr = context.mkRealConst(name)

  override def mkRealConst(name: String): RealExpr = context.mkRealConst(name)

  /**
    * Creates a bit-vector constant.
    **/
  override def mkBVConst(name: Symbol, size: Int): BitVecExpr = context.mkBVConst(name, size)

  override def mkBVConst(name: String, size: Int): BitVecExpr = context.mkBVConst(name, size)

  /**
    * Create a new function application.
    **/
  override def mkApp(f: FuncDecl, args: Expr*): Expr = context.mkApp(f, args:_*)

  /**
    * The true Term.
    **/
  override def mkTrue: BoolExpr = context.mkTrue

  /**
    * The false Term.
    **/
  override def mkFalse: BoolExpr = context.mkFalse

  /**
    * Creates a Boolean value.
    **/
  override def mkBool(value: Boolean): BoolExpr = context.mkBool(value)

  /**
    * Creates the equality {@code x"/> = <paramref name="y}.
    **/
  override def mkEq(x: Expr, y: Expr): BoolExpr = context.mkEq(x, y)

  /**
    * Creates a {@code distinct} term.
    **/
  override def mkDistinct(args: Expr*): BoolExpr = context.mkDistinct(args:_*)

  /**
    * Mk an expression representing {@code not(a)}.
    **/
  override def mkNot(a: BoolExpr): BoolExpr = context.mkNot(a)

  /**
    * Create an expression representing an if-then-else:
    * {@code ite(t1, t2, t3)}.
    *
    * @param t1 An expression with Boolean sort
    * @param t2 An expression
    * @param t3 An expression with the same sort as { @code t2}
    **/
  override def mkITE(t1: BoolExpr, t2: Expr, t3: Expr): Expr = context.mkITE(t1, t2, t3)

  /**
    * Create an expression representing {@code t1 iff t2}.
    **/
  override def mkIff(t1: BoolExpr, t2: BoolExpr): BoolExpr = context.mkIff(t1, t2)

  /**
    * Create an expression representing {@code t1 -> t2}.
    **/
  override def mkImplies(t1: BoolExpr, t2: BoolExpr): BoolExpr = context.mkImplies(t1, t2)

  /**
    * Create an expression representing {@code t1 xor t2}.
    **/
  override def mkXor(t1: BoolExpr, t2: BoolExpr): BoolExpr = context.mkXor(t1, t2)

  /**
    * Create an expression representing {@code t[0] and t[1] and ...}.
    **/
  override def mkAnd(t: BoolExpr*): BoolExpr = context.mkAnd(t:_*)

  /**
    * Create an expression representing {@code t[0] or t[1] or ...}.
    **/
  override def mkOr(t: BoolExpr*): BoolExpr = context.mkOr(t:_*)

  /**
    * Create an expression representing {@code t[0] + t[1] + ...}.
    **/
  override def mkAdd(t: ArithExpr*): ArithExpr = context.mkAdd(t:_*)

  /**
    * Create an expression representing {@code t[0] * t[1] * ...}.
    **/
  override def mkMul(t: ArithExpr*): ArithExpr = context.mkMul(t:_*)

  /**
    * Create an expression representing {@code t[0] - t[1] - ...}.
    **/
  override def mkSub(t: ArithExpr*): ArithExpr = context.mkSub(t:_*)

  /**
    * Create an expression representing {@code -t}.
    **/
  override def mkUnaryMinus(t: ArithExpr): ArithExpr = context.mkUnaryMinus(t)

  /**
    * Create an expression representing {@code t1 / t2}.
    **/
  override def mkDiv(t1: ArithExpr, t2: ArithExpr): ArithExpr = context.mkDiv(t1, t2)

  /**
    * Create an expression representing {@code t1 mod t2}.
    * Remarks: The
    * arguments must have int type.
    **/
  override def mkMod(t1: IntExpr, t2: IntExpr): IntExpr = context.mkMod(t1, t2)

  /**
    * Create an expression representing {@code t1 rem t2}.
    * Remarks: The
    * arguments must have int type.
    **/
  override def mkRem(t1: IntExpr, t2: IntExpr): IntExpr = context.mkRem(t1, t2)

  /**
    * Create an expression representing {@code t1 ^ t2}.
    **/
  override def mkPower(t1: ArithExpr, t2: ArithExpr): ArithExpr = context.mkPower(t1, t2)

  /**
    * Create an expression representing {@code t1 &lt; t2}
    **/
  override def mkLt(t1: ArithExpr, t2: ArithExpr): BoolExpr = context.mkLt(t1, t2)

  /**
    * Create an expression representing {@code t1 &lt;= t2}
    **/
  override def mkLe(t1: ArithExpr, t2: ArithExpr): BoolExpr = context.mkLe(t1, t2)

  /**
    * Create an expression representing {@code t1 &gt; t2}
    **/
  override def mkGt(t1: ArithExpr, t2: ArithExpr): BoolExpr = context.mkGt(t1, t2)

  /**
    * Create an expression representing {@code t1 &gt;= t2}
    **/
  override def mkGe(t1: ArithExpr, t2: ArithExpr): BoolExpr = context.mkGe(t1, t2)

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
  override def mkInt2Real(t: IntExpr): RealExpr = context.mkInt2Real(t)

  /**
    * Coerce a real to an integer.
    * Remarks:  The semantics of this function
    * follows the SMT-LIB standard for the function to_int. The argument must
    * be of real sort.
    **/
  override def mkReal2Int(t: RealExpr): IntExpr = context.mkReal2Int(t)

  /**
    * Creates an expression that checks whether a real number is an integer.
    **/
  override def mkIsInteger(t: RealExpr): BoolExpr = context.mkIsInteger(t)

  /**
    * Bitwise negation.
    * Remarks: The argument must have a bit-vector
    * sort.
    **/
  override def mkBVNot(t: BitVecExpr): BitVecExpr = context.mkBVNot(t)

  /**
    * Take conjunction of bits in a vector, return vector of length 1.
    *
    * Remarks: The argument must have a bit-vector sort.
    **/
  override def mkBVRedAND(t: BitVecExpr): BitVecExpr = context.mkBVRedAND(t)

  /**
    * Take disjunction of bits in a vector, return vector of length 1.
    *
    * Remarks: The argument must have a bit-vector sort.
    **/
  override def mkBVRedOR(t: BitVecExpr): BitVecExpr = context.mkBVRedOR(t)

  /**
    * Bitwise conjunction.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  override def mkBVAND(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVAND(t1, t2)

  /**
    * Bitwise disjunction.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  override def mkBVOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVOR(t1, t2)

  /**
    * Bitwise XOR.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  override def mkBVXOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVXOR(t1, t2)

  /**
    * Bitwise NAND.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  override def mkBVNAND(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVNAND(t1, t2)

  /**
    * Bitwise NOR.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  override def mkBVNOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVNOR(t1, t2)

  /**
    * Bitwise XNOR.
    * Remarks: The arguments must have a bit-vector
    * sort.
    **/
  override def mkBVXNOR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVXNOR(t1, t2)

  /**
    * Standard two's complement unary minus.
    * Remarks: The arguments must have a
    * bit-vector sort.
    **/
  override def mkBVNeg(t: BitVecExpr): BitVecExpr = context.mkBVNeg(t)

  /**
    * Two's complement addition.
    * Remarks: The arguments must have the same
    * bit-vector sort.
    **/
  override def mkBVAdd(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVAdd(t1, t2)

  /**
    * Two's complement subtraction.
    * Remarks: The arguments must have the same
    * bit-vector sort.
    **/
  override def mkBVSub(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVSub(t1, t2)

  /**
    * Two's complement multiplication.
    * Remarks: The arguments must have the
    * same bit-vector sort.
    **/
  override def mkBVMul(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVMul(t1, t2)

  /**
    * Unsigned division.
    * Remarks:  It is defined as the floor of
    * {@code t1/t2} if \c t2 is different from zero. If {@code t2} is
    * zero, then the result is undefined. The arguments must have the same
    * bit-vector sort.
    **/
  override def mkBVUDiv(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVUDiv(t1, t2)

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
  override def mkBVSDiv(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVSDiv(t1, t2)

  /**
    * Unsigned remainder.
    * Remarks:  It is defined as
    * {@code t1 - (t1 /u t2) * t2}, where {@code /u} represents
    * unsigned division. If {@code t2} is zero, then the result is
    * undefined. The arguments must have the same bit-vector sort.
    **/
  override def mkBVURem(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVURem(t1, t2)

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
  override def mkBVSRem(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVSRem(t1, t2)

  /**
    * Two's complement signed remainder (sign follows divisor).
    * Remarks:  If
    * {@code t2} is zero, then the result is undefined. The arguments must
    * have the same bit-vector sort.
    **/
  override def mkBVSMod(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVSMod(t1, t2)

  /**
    * Unsigned less-than
    * Remarks:  The arguments must have the same bit-vector
    * sort.
    **/
  override def mkBVULT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVULT(t1, t2)

  /**
    * Two's complement signed less-than
    * Remarks:  The arguments must have the
    * same bit-vector sort.
    **/
  override def mkBVSLT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVSLT(t1, t2)

  /**
    * Unsigned less-than or equal to.
    * Remarks:  The arguments must have the
    * same bit-vector sort.
    **/
  override def mkBVULE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVULE(t1, t2)

  /**
    * Two's complement signed less-than or equal to.
    * Remarks:  The arguments
    * must have the same bit-vector sort.
    **/
  override def mkBVSLE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVSLE(t1, t2)

  /**
    * Unsigned greater than or equal to.
    * Remarks:  The arguments must have the
    * same bit-vector sort.
    **/
  override def mkBVUGE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVUGE(t1, t2)

  /**
    * Two's complement signed greater than or equal to.
    * Remarks:  The arguments
    * must have the same bit-vector sort.
    **/
  override def mkBVSGE(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVSGE(t1, t2)

  /**
    * Unsigned greater-than.
    * Remarks:  The arguments must have the same
    * bit-vector sort.
    **/
  override def mkBVUGT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVUGT(t1, t2)

  /**
    * Two's complement signed greater-than.
    * Remarks:  The arguments must have
    * the same bit-vector sort.
    **/
  override def mkBVSGT(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVSGT(t1, t2)

  /**
    * Bit-vector concatenation.
    * Remarks:  The arguments must have a bit-vector
    * sort.
    *
    * @return The result is a bit-vector of size { @code n1+n2}, where
    *                                                    { @code n1} ({ @code n2}) is the size of { @code t1}
    *                                                    ({ @code t2}).
    *
    **/
  override def mkConcat(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkConcat(t1, t2)

  /**
    * Bit-vector extraction.
    * Remarks:  Extract the bits {@code high}
    * down to {@code low} from a bitvector of size {@code m} to
    * yield a new bitvector of size {@code n}, where
    * {@code n = high - low + 1}. The argument {@code t} must
    * have a bit-vector sort.
    **/
  override def mkExtract(high: Int, low: Int, t: BitVecExpr): BitVecExpr = context.mkExtract(high, low, t)

  /**
    * Bit-vector sign extension.
    * Remarks:  Sign-extends the given bit-vector to
    * the (signed) equivalent bitvector of size {@code m+i}, where \c m is
    * the size of the given bit-vector. The argument {@code t} must
    * have a bit-vector sort.
    **/
  override def mkSignExt(i: Int, t: BitVecExpr): BitVecExpr = context.mkSignExt(i, t)

  /**
    * Bit-vector zero extension.
    * Remarks:  Extend the given bit-vector with
    * zeros to the (unsigned) equivalent bitvector of size {@code m+i},
    * where \c m is the size of the given bit-vector. The argument {@code t}
    * must have a bit-vector sort.
    **/
  override def mkZeroExt(i: Int, t: BitVecExpr): BitVecExpr = context.mkZeroExt(i, t)

  /**
    * Bit-vector repetition.
    * Remarks:  The argument {@code t} must
    * have a bit-vector sort.
    **/
  override def mkRepeat(i: Int, t: BitVecExpr): BitVecExpr = context.mkRepeat(i, t)

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
  override def mkBVSHL(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVSHL(t1, t2)

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
  override def mkBVLSHR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVLSHR(t1, t2)

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
  override def mkBVASHR(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVASHR(t1, t2)

  /**
    * Rotate Left.
    * Remarks:  Rotate bits of \c t to the left \c i times. The
    * argument {@code t} must have a bit-vector sort.
    **/
  override def mkBVRotateLeft(i: Int, t: BitVecExpr): BitVecExpr = context.mkBVRotateLeft(i, t)

  /**
    * Rotate Right.
    * Remarks:  Rotate bits of \c t to the right \c i times. The
    * argument {@code t} must have a bit-vector sort.
    **/
  override def mkBVRotateRight(i: Int, t: BitVecExpr): BitVecExpr = context.mkBVRotateRight(i, t)

  /**
    * Rotate Left.
    * Remarks:  Rotate bits of {@code t1} to the left
    * {@code t2} times. The arguments must have the same bit-vector
    * sort.
    **/
  override def mkBVRotateLeft(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVRotateLeft(t1, t2)

  /**
    * Rotate Right.
    * Remarks:  Rotate bits of {@code t1} to the
    * right{@code t2} times. The arguments must have the same
    * bit-vector sort.
    **/
  override def mkBVRotateRight(t1: BitVecExpr, t2: BitVecExpr): BitVecExpr = context.mkBVRotateRight(t1, t2)

  /**
    * Create an {@code n} bit bit-vector from the integer argument
    * {@code t}.
    * Remarks:  NB. This function is essentially treated
    * as uninterpreted. So you cannot expect Z3 to precisely reflect the
    * semantics of this function when solving constraints with this function.
    *
    * The argument must be of integer sort.
    **/
  override def mkInt2BV(n: Int, t: IntExpr): BitVecExpr = context.mkInt2BV(n, t)

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
  override def mkBV2Int(t: BitVecExpr, signed: Boolean): IntExpr = context.mkBV2Int(t, signed)

  /**
    * Create a predicate that checks that the bit-wise addition does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVAddNoOverflow(t1: BitVecExpr, t2: BitVecExpr, isSigned: Boolean): BoolExpr = context.mkBVAddNoOverflow(t1, t2, isSigned)

  /**
    * Create a predicate that checks that the bit-wise addition does not
    * underflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVAddNoUnderflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVAddNoUnderflow(t1, t2)

  /**
    * Create a predicate that checks that the bit-wise subtraction does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVSubNoOverflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVSubNoOverflow(t1, t2)

  /**
    * Create a predicate that checks that the bit-wise subtraction does not
    * underflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVSubNoUnderflow(t1: BitVecExpr, t2: BitVecExpr, isSigned: Boolean): BoolExpr = context.mkBVSubNoUnderflow(t1, t2, isSigned)

  /**
    * Create a predicate that checks that the bit-wise signed division does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVSDivNoOverflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVSDivNoOverflow(t1, t2)

  /**
    * Create a predicate that checks that the bit-wise negation does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVNegNoOverflow(t: BitVecExpr): BoolExpr = context.mkBVNegNoOverflow(t)

  /**
    * Create a predicate that checks that the bit-wise multiplication does not
    * overflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVMulNoOverflow(t1: BitVecExpr, t2: BitVecExpr, isSigned: Boolean): BoolExpr = context.mkBVMulNoOverflow(t1, t2, isSigned)

  /**
    * Create a predicate that checks that the bit-wise multiplication does not
    * underflow.
    * Remarks:  The arguments must be of bit-vector sort.
    **/
  override def mkBVMulNoUnderflow(t1: BitVecExpr, t2: BitVecExpr): BoolExpr = context.mkBVMulNoUnderflow(t1, t2)

  /**
    * Create an array constant.
    **/
  override def mkArrayConst(name: Symbol, domain: Sort, range: Sort): ArrayExpr = context.mkArrayConst(name, domain, range)

  override def mkArrayConst(name: String, domain: Sort, range: Sort): ArrayExpr = context.mkArrayConst(name, domain, range)

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
  override def mkSelect(a: ArrayExpr, i: Expr): Expr = context.mkSelect(a, i)

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
  override def mkStore(a: ArrayExpr, i: Expr, v: Expr): ArrayExpr = context.mkStore(a, i, v)

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
  override def mkConstArray(domain: Sort, v: Expr): ArrayExpr = context.mkConstArray(domain, v)

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
  override def mkMap(f: FuncDecl, args: ArrayExpr*): ArrayExpr = context.mkMap(f, args:_*)

  /**
    * Access the array default value.
    * Remarks:  Produces the default range
    * value, for arrays that can be represented as finite maps with a default
    * range value.
    **/
  override def mkTermArray(array: ArrayExpr): Expr = context.mkTermArray(array)

  /**
    * Create Extentionality index. Two arrays are equal if and only if they are equal on the index returned by MkArrayExt.
    **/
  override def mkArrayExt(arg1: ArrayExpr, arg2: ArrayExpr): Expr = context.mkArrayExt(arg1, arg2)

  /**
    * Create a set type.
    **/
  override def mkSetSort(ty: Sort): SetSort = context.mkSetSort(ty)

  /**
    * Create an empty set.
    **/
  override def mkEmptySet(domain: Sort): ArrayExpr = context.mkEmptySet(domain)

  /**
    * Create the full set.
    **/
  override def mkFullSet(domain: Sort): ArrayExpr = context.mkFullSet(domain)

  /**
    * Add an element to the set.
    **/
  override def mkSetAdd(set: ArrayExpr, element: Expr): ArrayExpr = context.mkSetAdd(set, element)

  /**
    * Remove an element from a set.
    **/
  override def mkSetDel(set: ArrayExpr, element: Expr): ArrayExpr = context.mkSetDel(set, element)

  /**
    * Take the union of a list of sets.
    **/
  override def mkSetUnion(args: ArrayExpr*): ArrayExpr = context.mkSetUnion(args:_*)

  /**
    * Take the intersection of a list of sets.
    **/
  override def mkSetIntersection(args: ArrayExpr*): ArrayExpr = context.mkSetIntersection(args:_*)

  /**
    * Take the difference between two sets.
    **/
  override def mkSetDifference(arg1: ArrayExpr, arg2: ArrayExpr): ArrayExpr = context.mkSetDifference(arg1, arg2)

  /**
    * Take the complement of a set.
    **/
  override def mkSetComplement(arg: ArrayExpr): ArrayExpr = context.mkSetComplement(arg)

  /**
    * Check for set membership.
    **/
  override def mkSetMembership(elem: Expr, set: ArrayExpr): BoolExpr = context.mkSetMembership(elem, set)

  /**
    * Check for subsetness of sets.
    **/
  override def mkSetSubset(arg1: ArrayExpr, arg2: ArrayExpr): BoolExpr = context.mkSetSubset(arg1, arg2)

  /**
    * Create the empty sequence.
    *//**
    * Sequences, Strings and regular expressions.
    */
  override def MkEmptySeq(s: Sort): SeqExpr = context.mkEmptySeq(s)

  /**
    * Create the singleton sequence.
    */
  override def MkUnit(elem: Expr): SeqExpr = context.mkUnit(elem)

  /**
    * Create a string constant.
    */
  override def MkString(s: String): SeqExpr = context.mkString(s)

  /**
    * Concatentate sequences.
    */
  override def MkConcat(t: SeqExpr*): SeqExpr = context.mkConcat(t:_*)

  /**
    * Retrieve the length of a given sequence.
    */
  override def MkLength(s: SeqExpr): IntExpr = context.mkLength(s)

  /**
    * Check for sequence prefix.
    */
  override def MkPrefixOf(s1: SeqExpr, s2: SeqExpr): BoolExpr = context.mkPrefixOf(s1, s2)

  /**
    * Check for sequence suffix.
    */
  override def MkSuffixOf(s1: SeqExpr, s2: SeqExpr): BoolExpr = context.mkSuffixOf(s1, s2)

  /**
    * Check for sequence containment of s2 in s1.
    */
  override def MkContains(s1: SeqExpr, s2: SeqExpr): BoolExpr = context.mkContains(s1, s2)

  /**
    * Retrieve sequence of length one at index.
    */
  override def MkAt(s: SeqExpr, index: IntExpr): SeqExpr = context.mkAt(s, index)

  /**
    * Extract subsequence.
    */
  override def MkExtract(s: SeqExpr, offset: IntExpr, length: IntExpr): SeqExpr = context.mkExtract(s, offset, length)

  /**
    * Extract index of sub-string starting at offset.
    */
  override def MkIndexOf(s: SeqExpr, substr: SeqExpr, offset: ArithExpr): IntExpr = context.mkIndexOf(s, substr, offset)

  /**
    * Replace the first occurrence of src by dst in s.
    */
  override def MkReplace(s: SeqExpr, src: SeqExpr, dst: SeqExpr): SeqExpr = context.mkReplace(s, src, dst)

  /**
    * Convert a regular expression that accepts sequence s.
    */
  override def MkToRe(s: SeqExpr): ReExpr = context.mkToRe(s)

  /**
    * Check for regular expression membership.
    */
  override def MkInRe(s: SeqExpr, re: ReExpr): BoolExpr = context.mkInRe(s, re)

  /**
    * Take the Kleene star of a regular expression.
    */
  override def MkStar(re: ReExpr): ReExpr = context.mkStar(re)

  /**
    * Take the Kleene plus of a regular expression.
    */
  override def MPlus(re: ReExpr): ReExpr = context.mkPlus(re)

  /**
    * Create the optional regular expression.
    */
  override def MOption(re: ReExpr): ReExpr = context.mkOption(re)

  /**
    * Create the concatenation of regular languages.
    */
  override def MkConcatRE(t: ReExpr*): ReExpr = context.mkConcat(t:_*)

  /**
    * Create the union of regular languages.
    */
  override def MkUnion(t: ReExpr*): ReExpr = context.mkUnion(t:_*)

  /**
    * Create a Term of a given sort.
    *
    * @param v A string representing the term value in decimal notation. If the given sort is a real, then the
    *          Term can be a rational, that is, a string of the form
    *          { @code [num]* / [num]*}.
    * @param ty The sort of the
    * numeral. In the current implementation, the given sort can be an int,
    *           real, or bit-vectors of arbitrary size.
    * @return A Term with value { @code v} and sort { @code ty}
    **/
  override def mkNumeral(v: String, ty: Sort): Expr = context.mkNumeral(v, ty)

  /**
    * Create a Term of a given sort. This function can be use to create
    * numerals that fit in a machine integer. It is slightly faster than
    * {@code MakeNumeral} since it is not necessary to parse a string.
    *
    * @param v  Value of the numeral
    * @param ty Sort of the numeral
    * @return A Term with value { @code v} and type { @code ty}
    **/
  override def mkNumeral(v: Int, ty: Sort): Expr = context.mkNumeral(v, ty)

  override def mkNumeral(v: Long, ty: Sort): Expr = context.mkNumeral(v, ty)

  /**
    * Create a real from a fraction.
    *
    * @param num numerator of rational.
    * @param den denominator of rational.
    * @return A Term with value { @code num}/{ @code den}
    *                                   and sort Real
    * @see #mkNumeral(String,Sort)
    **/
  override def mkReal(num: Int, den: Int): RatNum = context.mkReal(num, den)

  /**
    * Create a real numeral.
    *
    * @param v A string representing the Term value in decimal notation.
    * @return A Term with value { @code v} and sort Real
    **/
  override def mkReal(v: String): RatNum = context.mkReal(v)

  /**
    * Create a real numeral.
    *
    * @param v value of the numeral.
    * @return A Term with value { @code v} and sort Real
    **/
  override def mkReal(v: Int): RatNum = context.mkReal(v)

  override def mkReal(v: Long): RatNum = context.mkReal(v)

  /**
    * Create an integer numeral.
    *
    * @param v A string representing the Term value in decimal notation.
    **/
  override def mkInt(v: String): IntNum = context.mkInt(v)

  /**
    * Create an integer numeral.
    *
    * @param v value of the numeral.
    * @return A Term with value { @code v} and sort Integer
    **/
  override def mkInt(v: Int): IntNum = context.mkInt(v)

  override def mkInt(v: Long): IntNum = context.mkInt(v)

  /**
    * Create a bit-vector numeral.
    *
    * @param v    A string representing the value in decimal notation.
    * @param size the size of the bit-vector
    **/
  override def mkBV(v: String, size: Int): BitVecNum = context.mkBV(v, size)

  /**
    * Create a bit-vector numeral.
    *
    * @param v    value of the numeral.
    * @param size the size of the bit-vector
    **/
  override def mkBV(v: Int, size: Int): BitVecNum = context.mkBV(v, size)

  /**
    * Create a bit-vector numeral.
    *
    * @param v    value of the numeral. *
    * @param size the size of the bit-vector
    **/
  override def mkBV(v: Long, size: Int): BitVecNum = context.mkBV(v, size)

  /**
    * Create a universal Quantifier.
    *
    * @param sorts    the sorts of the bound variables.
    * @param names    names of the bound variables
    * @param body     the body of the quantifier.
    * @param weight   quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
    * @param patterns array containing the patterns created using { @code MkPattern}.
    * @param noPatterns array containing the anti-patterns created using { @code MkPattern}.
    * @param quantifierID optional symbol to track quantifier.
    * @param skolemID     optional symbol to track skolem constants.
    * @return Creates a forall formula, where
    *         { @code weight} is the weight, { @code patterns} is
    *                 an array of patterns, { @code sorts} is an array with the sorts
    *                 of the bound variables, { @code names} is an array with the
    *                 'names' of the bound variables, and { @code body} is the body
    *                 of the quantifier. Quantifiers are associated with weights indicating the
    *                 importance of using the quantifier during instantiation.
    *                 Note that the bound variables are de-Bruijn indices created using { @link #mkBound}.
    *                                                                                           Z3 applies the convention that the last element in { @code names} and
    *                                                                                                                                                      { @code sorts} refers to the variable with index 0, the second to last element
    *                                                                                                                                                      of { @code names} and { @code sorts} refers to the variable
    *                                                                                                                                                      with index 1, etc.
    **/
  override def mkForall(sorts: Array[Sort], names: Array[Symbol], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier = context.mkForall(sorts, names, body, weight, patterns, noPatterns, quantifierID, skolemID)

  /**
    * Creates a universal quantifier using a list of constants that will form the set of bound variables.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  override def mkForall(boundConstants: Array[Expr], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier = context.mkForall(boundConstants, body, weight, patterns, noPatterns, quantifierID, skolemID)

  /**
    * Creates an existential quantifier using de-Brujin indexed variables.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  override def mkExists(sorts: Array[Sort], names: Array[Symbol], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier = context.mkExists(sorts, names, body, weight, patterns, noPatterns, quantifierID, skolemID)

  /**
    * Creates an existential quantifier using a list of constants that will form the set of bound variables.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  override def mkExists(boundConstants: Array[Expr], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier = context.mkExists(boundConstants, body, weight, patterns, noPatterns, quantifierID, skolemID)

  /**
    * Create a Quantifier.
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  override def mkQuantifier(universal: Boolean, sorts: Array[Sort], names: Array[Symbol], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier = context.mkQuantifier(universal, sorts, names, body, weight, patterns, noPatterns, quantifierID, skolemID)

  /**
    * Create a Quantifier
    *
    * @see #mkForall(Sort[],Symbol[],Expr,int,Pattern[],Expr[],Symbol,Symbol)
    **/
  override def mkQuantifier(universal: Boolean, boundConstants: Array[Expr], body: Expr, weight: Int, patterns: Array[Pattern], noPatterns: Array[Expr], quantifierID: Symbol, skolemID: Symbol): z3.Quantifier = context.mkQuantifier(universal, boundConstants, body, weight, patterns, noPatterns, quantifierID, skolemID)

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
  override def setPrintMode(value: Z3_ast_print_mode): Unit = context.setPrintMode(value)

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
  override def benchmarkToSMTString(name: String, logic: String, status: String, attributes: String, assumptions: Array[BoolExpr], formula: BoolExpr): String = context.benchmarkToSMTString(name, logic, status, attributes, assumptions, formula)

  /**
    * Parse the given string using the SMT-LIB parser.
    * Remarks:  The symbol
    * table of the parser can be initialized using the given sorts and
    * declarations. The symbols in the arrays {@code sortNames} and
    * {@code declNames} don't need to match the names of the sorts
    * and declarations in the arrays {@code sorts} and {@code decls}. This is a useful feature since we can use arbitrary names
    * to reference sorts and declarations.
    **/
  override def parseSMTLIBString(str: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Unit = context.parseSMTLIB2String(str, sortNames, sorts, declNames, decls)

  /**
    * Parse the given file using the SMT-LIB parser.
    *
    * @see #parseSMTLIBString
    **/
  override def parseSMTLIBFile(fileName: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Unit = context.parseSMTLIB2File(fileName, sortNames, sorts, declNames, decls)


  /**
    * Parse the given string using the SMT-LIB2 parser.
    *
    * @see #parseSMTLIBString
    * @return A conjunction of assertions in the scope (up to push/pop) at the
    *         end of the string.
    **/
  override def parseSMTLIB2String(str: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Array[BoolExpr] = context.parseSMTLIB2String(str, sortNames, sorts, declNames, decls)

  /**
    * Parse the given file using the SMT-LIB2 parser.
    *
    * @see #parseSMTLIB2String
    **/
  override def parseSMTLIB2File(fileName: String, sortNames: Array[Symbol], sorts: Array[Sort], declNames: Array[Symbol], decls: Array[FuncDecl]): Array[BoolExpr] = context.parseSMTLIB2File(fileName, sortNames, sorts, declNames, decls)

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
  override def mkGoal(models: Boolean, unsatCores: Boolean, proofs: Boolean): Goal = context.mkGoal(models, unsatCores, proofs)

  /**
    * Creates a new ParameterSet.
    **/
  override def mkParams: Params = context.mkParams

  /**
    * The number of supported tactics.
    **/
  override def getNumTactics: Int = context.getNumTactics

  /**
    * The names of all supported tactics.
    **/
  override def getTacticNames: Array[String] = context.getTacticNames

  /**
    * Returns a string containing a description of the tactic with the given
    * name.
    **/
  override def getTacticDescription(name: String): String = context.getTacticDescription(name)

  /**
    * Creates a new Tactic.
    **/
  override def mkTactic(name: String): Tactic = context.mkTactic(name)

  /**
    * Create a tactic that applies {@code t1} to a Goal and then
    * {@code t2"/> to every subgoal produced by <paramref name="t1}.
    **/
  override def andThen(t1: Tactic, t2: Tactic, ts: Tactic*): Tactic = context.andThen(t1, t2, ts:_*)

  /**
    * Create a tactic that applies {@code t1} to a Goal and then
    * {@code t2} to every subgoal produced by {@code t1}
    *
    * Remarks:  Shorthand for {@code AndThen}.
    **/
  override def then(t1: Tactic, t2: Tactic, ts: Tactic*): Tactic = context.then(t1, t2, ts:_*)

  /**
    * Create a tactic that first applies {@code t1} to a Goal and if
    * it fails then returns the result of {@code t2} applied to the
    * Goal.
    **/
  override def orElse(t1: Tactic, t2: Tactic): Tactic = context.orElse(t1, t2)

  /**
    * Create a tactic that applies {@code t} to a goal for {@code ms} milliseconds.
    * Remarks:  If {@code t} does not
    * terminate within {@code ms} milliseconds, then it fails.
    *
    **/
  override def tryFor(t: Tactic, ms: Int): Tactic = context.tryFor(t, ms)

  /**
    * Create a tactic that applies {@code t} to a given goal if the
    * probe {@code p} evaluates to true.
    * Remarks:  If {@code p} evaluates to false, then the new tactic behaves like the
    * {@code skip} tactic.
    **/
  override def when(p: Probe, t: Tactic): Tactic = context.when(p, t)

  /**
    * Create a tactic that applies {@code t1} to a given goal if the
    * probe {@code p"/> evaluates to true and <paramref name="t2}
    * otherwise.
    **/
  override def cond(p: Probe, t1: Tactic, t2: Tactic): Tactic = context.cond(p, t1, t2)

  /**
    * Create a tactic that keeps applying {@code t} until the goal
    * is not modified anymore or the maximum number of iterations {@code max} is reached.
    **/
  override def repeat(t: Tactic, max: Int): Tactic = context.repeat(t, max)

  /**
    * Create a tactic that just returns the given goal.
    **/
  override def skip: Tactic = context.skip

  /**
    * Create a tactic always fails.
    **/
  override def fail: Tactic = context.fail

  /**
    * Create a tactic that fails if the probe {@code p} evaluates to
    * false.
    **/
  override def failIf(p: Probe): Tactic = context.failIf(p)

  /**
    * Create a tactic that fails if the goal is not triviall satisfiable (i.e.,
    * empty) or trivially unsatisfiable (i.e., contains `false').
    **/
  override def failIfNotDecided: Tactic = context.failIfNotDecided

  /**
    * Create a tactic that applies {@code t} using the given set of
    * parameters {@code p}.
    **/
  override def usingParams(t: Tactic, p: Params): Tactic = context.usingParams(t, p)

  /**
    * Create a tactic that applies {@code t} using the given set of
    * parameters {@code p}.
    * Remarks: Alias for
    * {@code UsingParams}
    **/
  override def `with`(t: Tactic, p: Params): Tactic = context.`with`(t, p)

  /**
    * Create a tactic that applies the given tactics in parallel until one of them succeeds (i.e., the first that doesn't fail).
    **/
  override def parOr(t: Tactic*): Tactic = context.parOr(t:_*)

  /**
    * Create a tactic that applies {@code t1} to a given goal and
    * then {@code t2} to every subgoal produced by {@code t1}. The subgoals are processed in parallel.
    **/
  override def parAndThen(t1: Tactic, t2: Tactic): Tactic = context.parAndThen(t1, t2)

  /**
    * Interrupt the execution of a Z3 procedure.
    * Remarks: This procedure can be
    * used to interrupt: solvers, simplifiers and tactics.
    **/
  override def interrupt(): Unit = context.interrupt()

  /**
    * The number of supported Probes.
    **/
  override def getNumProbes: Int = context.getNumProbes

  /**
    * The names of all supported Probes.
    **/
  override def getProbeNames: Array[String] = context.getProbeNames

  /**
    * Returns a string containing a description of the probe with the given
    * name.
    **/
  override def getProbeDescription(name: String): String = context.getProbeDescription(name)

  /**
    * Creates a new Probe.
    **/
  override def mkProbe(name: String): Probe = context.mkProbe(name)

  /**
    * Create a probe that always evaluates to {@code val}.
    **/
  override def constProbe(`val`: Double): Probe = context.constProbe(`val`)

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is less than the value returned by {@code p2}
    **/
  override def lt(p1: Probe, p2: Probe): Probe = context.lt(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is greater than the value returned by {@code p2}
    **/
  override def gt(p1: Probe, p2: Probe): Probe = context.gt(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is less than or equal the value returned by
    * {@code p2}
    **/
  override def le(p1: Probe, p2: Probe): Probe = context.le(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is greater than or equal the value returned by
    * {@code p2}
    **/
  override def ge(p1: Probe, p2: Probe): Probe = context.ge(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value returned by
    * {@code p1} is equal to the value returned by {@code p2}
    **/
  override def eq(p1: Probe, p2: Probe): Probe = context.eq(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value {@code p1} and {@code p2} evaluate to "true".
    **/
  override def and(p1: Probe, p2: Probe): Probe = context.and(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value {@code p1} or {@code p2} evaluate to "true".
    **/
  override def or(p1: Probe, p2: Probe): Probe = context.or(p1, p2)

  /**
    * Create a probe that evaluates to "true" when the value {@code p} does not evaluate to "true".
    **/
  override def not(p: Probe): Probe = context.not(p)

  /**
    * Creates a new (incremental) solver.
    * Remarks:  This solver also uses a set
    * of builtin tactics for handling the first check-sat command, and
    * check-sat commands that take more than a given number of milliseconds to
    * be solved.
    **/
  override def mkSolver: Solver = context.mkSolver

  override def mkSolver(logic: Symbol): Solver = context.mkSolver(logic)

  /**
    * Creates a new (incremental) solver.
    *
    * @see #mkSolver(Symbol)
    **/
  override def mkSolver(logic: String): Solver = context.mkSolver(logic)

  /**
    * Creates a new (incremental) solver.
    **/
  override def mkSimpleSolver: Solver = context.mkSimpleSolver

  /**
    * Creates a solver that is implemented using the given tactic.
    * Remarks:
    * The solver supports the commands {@code Push} and {@code Pop},
    * but it will always solve each check from scratch.
    **/
  override def mkSolver(t: Tactic): Solver = context.mkSolver(t)

  /**
    * Create a Fixedpoint context.
    **/
  override def mkFixedpoint: Fixedpoint = context.mkFixedpoint

  /**
    * Create a Optimize context.
    **/
  override def mkOptimize: Optimize = context.mkOptimize

  /**
    * Create the floating-point RoundingMode sort.
    *
    * @throws Z3Exception
    **/
  override def mkFPRoundingModeSort: FPRMSort = context.mkFPRoundingModeSort

  /**
    * Create a numeral of RoundingMode sort which represents the NearestTiesToEven rounding mode.
    *
    * @throws Z3Exception
    **/
  override def mkFPRoundNearestTiesToEven: FPRMExpr = context.mkFPRoundNearestTiesToEven

  override def mkFPRNE: FPRMNum = context.mkFPRNE

  /**
    * Create a numeral of RoundingMode sort which represents the NearestTiesToAway rounding mode.
    *
    * @throws Z3Exception
    **/
  override def mkFPRoundNearestTiesToAway: FPRMNum = context.mkFPRoundNearestTiesToAway

  override def mkFPRNA: FPRMNum = context.mkFPRNA

  /**
    * Create a numeral of RoundingMode sort which represents the RoundTowardPositive rounding mode.
    *
    * @throws Z3Exception
    **/
  override def mkFPRoundTowardPositive: FPRMNum = context.mkFPRoundTowardPositive

  override def mkFPRTP: FPRMNum = context.mkFPRTP

  /**
    * Create a numeral of RoundingMode sort which represents the RoundTowardNegative rounding mode.
    *
    * @throws Z3Exception
    **/
  override def mkFPRoundTowardNegative: FPRMNum = context.mkFPRoundTowardNegative

  override def mkFPRTN: FPRMNum = context.mkFPRTN

  /**
    * Create a numeral of RoundingMode sort which represents the RoundTowardZero rounding mode.
    *
    * @throws Z3Exception
    **/
  override def mkFPRoundTowardZero: FPRMNum = context.mkFPRoundTowardZero

  override def mkFPRTZ: FPRMNum = context.mkFPRTZ

  /**
    * Create a FloatingPoint sort.
    *
    * @param ebits exponent bits in the FloatingPoint sort.
    * @param sbits significand bits in the FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFPSort(ebits: Int, sbits: Int): FPSort = context.mkFPSort(ebits, sbits)

  /**
    * Create the half-precision (16-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  override def mkFPSortHalf: FPSort = context.mkFPSortHalf

  override def mkFPSort16: FPSort = context.mkFPSort16

  /**
    * Create the single-precision (32-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  override def mkFPSortSingle: FPSort = context.mkFPSortSingle

  override def mkFPSort32: FPSort = context.mkFPSort32

  /**
    * Create the double-precision (64-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  override def mkFPSortDouble: FPSort = context.mkFPSortDouble

  override def mkFPSort64: FPSort = context.mkFPSort64

  /**
    * Create the quadruple-precision (128-bit) FloatingPoint sort.
    *
    * @throws Z3Exception
    **/
  override def mkFPSortQuadruple: FPSort = context.mkFPSortQuadruple

  override def mkFPSort128: FPSort = context.mkFPSort128

  /**
    * Create a NaN of sort s.
    *
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFPNaN(s: FPSort): FPNum = context.mkFPNaN(s)

  /**
    * Create a floating-point infinity of sort s.
    *
    * @param s        FloatingPoint sort.
    * @param negative indicates whether the result should be negative.
    * @throws Z3Exception
    **/
  override def mkFPInf(s: FPSort, negative: Boolean): FPNum = context.mkFPInf(s, negative)

  /**
    * Create a floating-point zero of sort s.
    *
    * @param s        FloatingPoint sort.
    * @param negative indicates whether the result should be negative.
    * @throws Z3Exception
    **/
  override def mkFPZero(s: FPSort, negative: Boolean): FPNum = context.mkFPZero(s, negative)

  /**
    * Create a numeral of FloatingPoint sort from a float.
    *
    * @param v numeral value.
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFPNumeral(v: Float, s: FPSort): FPNum = context.mkFPNumeral(v, s)

  override def mkFPNumeral(v: Double, s: FPSort): FPNum = context.mkFPNumeral(v, s)

  /**
    * Create a numeral of FloatingPoint sort from an int.
    * * @param v numeral value.
    *
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFPNumeral(v: Int, s: FPSort): FPNum = context.mkFPNumeral(v, s)

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two integers.
    *
    * @param sgn the sign.
    * @param sig the significand.
    * @param exp the exponent.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFPNumeral(sgn: Boolean, exp: Int, sig: Int, s: FPSort): FPNum = context.mkFPNumeral(sgn, exp, sig, s)

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two 64-bit integers.
    *
    * @param sgn the sign.
    * @param sig the significand.
    * @param exp the exponent.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFPNumeral(sgn: Boolean, exp: Long, sig: Long, s: FPSort): FPNum = context.mkFPNumeral(sgn, exp, sig, s)

  override def mkFP(v: Float, s: FPSort): FPNum = context.mkFP(v, s)

  override def mkFP(v: Double, s: FPSort): FPNum = context.mkFP(v, s)

  /**
    * Create a numeral of FloatingPoint sort from an int.
    *
    * @param v numeral value.
    * @param s FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFP(v: Int, s: FPSort): FPNum = context.mkFP(v, s)

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two integers.
    *
    * @param sgn the sign.
    * @param exp the exponent.
    * @param sig the significand.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFP(sgn: Boolean, exp: Int, sig: Int, s: FPSort): FPNum = context.mkFP(sgn, exp, sig, s)

  /**
    * Create a numeral of FloatingPoint sort from a sign bit and two 64-bit integers.
    *
    * @param sgn the sign.
    * @param exp the exponent.
    * @param sig the significand.
    * @param s   FloatingPoint sort.
    * @throws Z3Exception
    **/
  override def mkFP(sgn: Boolean, exp: Long, sig: Long, s: FPSort): FPNum = context.mkFP(sgn, exp, sig, s)

  /**
    * Floating-point absolute value
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPAbs(t: FPExpr): FPExpr = context.mkFPAbs(t)

  /**
    * Floating-point negation
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPNeg(t: FPExpr): FPExpr = context.mkFPNeg(t)

  /**
    * Floating-point addition
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPAdd(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPAdd(rm, t1, t2)

  /**
    * Floating-point subtraction
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPSub(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPSub(rm, t1, t2)

  /**
    * Floating-point multiplication
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPMul(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPMul(rm, t1, t2)

  /**
    * Floating-point division
    *
    * @param rm rounding mode term
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPDiv(rm: FPRMExpr, t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPDiv(rm, t1, t2)

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
  override def mkFPFMA(rm: FPRMExpr, t1: FPExpr, t2: FPExpr, t3: FPExpr): FPExpr = context.mkFPFMA(rm, t1, t2, t3)

  /**
    * Floating-point square root
    *
    * @param rm rounding mode term
    * @param t  floating-point term
    * @throws Z3Exception
    **/
  override def mkFPSqrt(rm: FPRMExpr, t: FPExpr): FPExpr = context.mkFPSqrt(rm, t)

  /**
    * Floating-point remainder
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPRem(t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPRem(t1, t2)

  /**
    * Floating-point roundToIntegral. Rounds a floating-point number to
    * the closest integer, again represented as a floating-point number.
    *
    * @param rm term of RoundingMode sort
    * @param t  floating-point term
    * @throws Z3Exception
    **/
  override def mkFPRoundToIntegral(rm: FPRMExpr, t: FPExpr): FPExpr = context.mkFPRoundToIntegral(rm, t)

  /**
    * Minimum of floating-point numbers.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPMin(t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPMin(t1, t2)

  /**
    * Maximum of floating-point numbers.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPMax(t1: FPExpr, t2: FPExpr): FPExpr = context.mkFPMax(t1, t2)

  /**
    * Floating-point less than or equal.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPLEq(t1: FPExpr, t2: FPExpr): BoolExpr = context.mkFPLEq(t1, t2)

  /**
    * Floating-point less than.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPLt(t1: FPExpr, t2: FPExpr): BoolExpr = context.mkFPLt(t1, t2)

  /**
    * Floating-point greater than or equal.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPGEq(t1: FPExpr, t2: FPExpr): BoolExpr = context.mkFPGEq(t1, t2)

  /**
    * Floating-point greater than.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    * @throws Z3Exception
    **/
  override def mkFPGt(t1: FPExpr, t2: FPExpr): BoolExpr = context.mkFPGt(t1, t2)

  /**
    * Floating-point equality.
    *
    * @param t1 floating-point term
    * @param t2 floating-point term
    *           Remarks:
    *           Note that this is IEEE 754 equality (as opposed to standard =).
    * @throws Z3Exception
    **/
  override def mkFPEq(t1: FPExpr, t2: FPExpr): BoolExpr = context.mkFPEq(t1, t2)

  /**
    * Predicate indicating whether t is a normal floating-point number.\
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsNormal(t: FPExpr): BoolExpr = context.mkFPIsNormal(t)

  /**
    * Predicate indicating whether t is a subnormal floating-point number.\
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsSubnormal(t: FPExpr): BoolExpr = context.mkFPIsSubnormal(t)

  /**
    * Predicate indicating whether t is a floating-point number with zero value, i.e., +0 or -0.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsZero(t: FPExpr): BoolExpr = context.mkFPIsZero(t)

  /**
    * Predicate indicating whether t is a floating-point number representing +oo or -oo.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsInfinite(t: FPExpr): BoolExpr = context.mkFPIsInfinite(t)

  /**
    * Predicate indicating whether t is a NaN.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsNaN(t: FPExpr): BoolExpr = context.mkFPIsNaN(t)

  /**
    * Predicate indicating whether t is a negative floating-point number.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsNegative(t: FPExpr): BoolExpr = context.mkFPIsNegative(t)

  /**
    * Predicate indicating whether t is a positive floating-point number.
    *
    * @param t floating-point term
    * @throws Z3Exception
    **/
  override def mkFPIsPositive(t: FPExpr): BoolExpr = context.mkFPIsPositive(t)

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
  override def mkFP(sgn: BitVecExpr, sig: BitVecExpr, exp: BitVecExpr): FPExpr = context.mkFP(sgn, sig, exp)

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
  override def mkFPToFP(bv: BitVecExpr, s: FPSort): FPExpr = context.mkFPToFP(bv, s)

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
  override def mkFPToFP(rm: FPRMExpr, t: FPExpr, s: FPSort): FPExpr = context.mkFPToFP(rm, t, s)

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
  override def mkFPToFP(rm: FPRMExpr, t: RealExpr, s: FPSort): FPExpr = context.mkFPToFP(rm, t, s)

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
  override def mkFPToFP(rm: FPRMExpr, t: BitVecExpr, s: FPSort, signed: Boolean): FPExpr = context.mkFPToFP(rm, t, s, signed)

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
  override def mkFPToFP(s: FPSort, rm: FPRMExpr, t: FPExpr): FPExpr = context.mkFPToFP(s, rm, t)

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
  override def mkFPToBV(rm: FPRMExpr, t: FPExpr, sz: Int, signed: Boolean): BitVecExpr = context.mkFPToBV(rm, t, sz, signed)

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
  override def mkFPToReal(t: FPExpr): RealExpr = context.mkFPToReal(t)

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
  override def mkFPToIEEEBV(t: FPExpr): BitVecExpr = context.mkFPToIEEEBV(t)

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
  override def mkFPToFP(rm: FPRMExpr, exp: IntExpr, sig: RealExpr, s: FPSort): BitVecExpr = context.mkFPToFP(rm, exp, sig, s)

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
  override def wrapAST(nativeObject: Long): AST = context.wrapAST(nativeObject)

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
  override def unwrapAST(a: AST): Long = context.unwrapAST(a)

  /**
    * Return a string describing all available parameters to
    * {@code Expr.Simplify}.
    **/
  override def SimplifyHelp: String = context.SimplifyHelp

  /**
    * Retrieves parameter descriptions for simplifier.
    **/
  override def getSimplifyParameterDescriptions: ParamDescrs = context.getSimplifyParameterDescriptions

  /**
    * Update a mutable configuration parameter.
    * Remarks:  The list of all
    * configuration parameters can be obtained using the Z3 executable:
    * {@code z3.exe -ini?} Only a few configuration parameters are mutable
    * once the context is created. An exception is thrown when trying to modify
    * an immutable parameter.
    **/
  override def updateParamValue(id: String, value: String): Unit = context.updateParamValue(id, value)

  override def getConstructorDRQ: IDecRefQueue[Constructor] = context.getConstructorDRQ

  override def getConstructorListDRQ: IDecRefQueue[ConstructorList] = context.getConstructorListDRQ

  override def getASTDRQ: IDecRefQueue[AST] = context.getASTDRQ

  override def getASTMapDRQ: IDecRefQueue[_ <: Z3Object] = context.getASTMapDRQ.asInstanceOf[IDecRefQueue[_ <: Z3Object]]

  override def getASTVectorDRQ: IDecRefQueue[ASTVector] = context.getASTVectorDRQ

  override def getApplyResultDRQ: IDecRefQueue[ApplyResult] = context.getApplyResultDRQ

  override def getFuncEntryDRQ: IDecRefQueue[FuncInterp.Entry] = context.getFuncEntryDRQ

  override def getFuncInterpDRQ: IDecRefQueue[FuncInterp] = context.getFuncInterpDRQ

  override def getGoalDRQ: IDecRefQueue[Goal] = context.getGoalDRQ

  override def getModelDRQ: IDecRefQueue[Model] = context.getModelDRQ

  override def getParamsDRQ: IDecRefQueue[Params] = context.getParamsDRQ

  override def getParamDescrsDRQ: IDecRefQueue[ParamDescrs] = context.getParamDescrsDRQ

  override def getProbeDRQ: IDecRefQueue[Probe] = context.getProbeDRQ

  override def getSolverDRQ: IDecRefQueue[Solver] = context.getSolverDRQ

  override def getStatisticsDRQ: IDecRefQueue[Statistics] = context.getStatisticsDRQ

  override def getTacticDRQ: IDecRefQueue[Tactic] = context.getTacticDRQ

  override def getFixedpointDRQ: IDecRefQueue[Fixedpoint] = context.getFixedpointDRQ

  override def getOptimizeDRQ: IDecRefQueue[Optimize] = context.getOptimizeDRQ

  /**
    * Disposes of the context.
    **/
  override def close(): Unit = context.close()

  override def mkLambda(sorts: Array[Sort], symbols: Array[Symbol], expr: BoolExpr): Expr = context.mkLambda(sorts, symbols, expr)
}