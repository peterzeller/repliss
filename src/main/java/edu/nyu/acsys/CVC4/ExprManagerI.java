package edu.nyu.acsys.CVC4;

public interface ExprManagerI {
    static boolean hasOperator(Kind k) {
      return CVC4JNI.ExprManager_hasOperator(k.swigValue());
    }

    static Type exportType(Type t, ExprManager em, ExprManagerMapCollection vmap) {
      return new Type(CVC4JNI.ExprManager_exportType(Type.getCPtr(t), t, ExprManager.getCPtr(em), em, ExprManagerMapCollection.getCPtr(vmap), vmap), true);
    }

    static long minArity(Kind kind) {
      return CVC4JNI.ExprManager_minArity(kind.swigValue());
    }

    static long maxArity(Kind kind) {
      return CVC4JNI.ExprManager_maxArity(kind.swigValue());
    }

    void delete();

    Options getOptions();

    ResourceManager getResourceManager();

    BooleanType booleanType();

    StringType stringType();

    RegExpType regExpType();

    RealType realType();

    IntegerType integerType();

    RoundingModeType roundingModeType();

    Expr mkExpr(Kind kind, Expr child1);

    Expr mkExpr(Kind kind, Expr child1, Expr child2);

    Expr mkExpr(Kind kind, Expr child1, Expr child2, Expr child3);

    Expr mkExpr(Kind kind, Expr child1, Expr child2, Expr child3, Expr child4);

    Expr mkExpr(Kind kind, Expr child1, Expr child2, Expr child3, Expr child4, Expr child5);

    Expr mkExpr(Kind kind, vectorExpr children);

    Expr mkExpr(Kind kind, Expr child1, vectorExpr otherChildren);

    Expr mkExpr(Expr opExpr);

    Expr mkExpr(Expr opExpr, Expr child1);

    Expr mkExpr(Expr opExpr, Expr child1, Expr child2);

    Expr mkExpr(Expr opExpr, Expr child1, Expr child2, Expr child3);

    Expr mkExpr(Expr opExpr, Expr child1, Expr child2, Expr child3, Expr child4);

    Expr mkExpr(Expr opExpr, Expr child1, Expr child2, Expr child3, Expr child4, Expr child5);

    Expr mkExpr(Expr opExpr, vectorExpr children);

    Expr mkAssociative(Kind kind, vectorExpr children);

    Expr operatorOf(Kind k);

    Kind operatorToKind(Expr e);

    FunctionType mkFunctionType(Type domain, Type range);

    FunctionType mkFunctionType(vectorType argTypes, Type range);

    FunctionType mkFunctionType(vectorType sorts);

    FunctionType mkPredicateType(vectorType sorts);

    DatatypeType mkTupleType(vectorType types);

    DatatypeType mkRecordType(Record rec);

    SExprType mkSExprType(vectorType types);

    FloatingPointType mkFloatingPointType(long exp, long sig);

    BitVectorType mkBitVectorType(long size);

    ArrayType mkArrayType(Type indexType, Type constituentType);

    SetType mkSetType(Type elementType);

    DatatypeType mkDatatypeType(Datatype datatype);

    vectorDatatypeType mkMutualDatatypeTypes(vectorDatatype datatypes);

    vectorDatatypeType mkMutualDatatypeTypes(vectorDatatype datatypes, setOfType unresolvedTypes);

    ConstructorType mkConstructorType(DatatypeConstructor constructor, Type range);

    SelectorType mkSelectorType(Type domain, Type range);

    TesterType mkTesterType(Type domain);

    SortType mkSort(String name, long flags);

    SortType mkSort(String name);

    SortConstructorType mkSortConstructor(String name, long arity);

    Type getType(Expr e, boolean check);

    Type getType(Expr e);

    Expr mkVar(String name, Type type, long flags);

    Expr mkVar(String name, Type type);

    Expr mkVar(Type type, long flags);

    Expr mkVar(Type type);

    Expr mkBoundVar(String name, Type type);

    Expr mkBoundVar(Type type);

    Expr mkNullaryOperator(Type type, Kind k);

    Statistics getStatistics();

    SExpr getStatistic(String name);

    void safeFlushStatistics(int fd);

    Expr mkConst(ArrayStoreAll arg0);

    Expr mkConst(BitVectorSize arg0);

    Expr mkConst(AscriptionType arg0);

    Expr mkConst(BitVectorBitOf arg0);

    Expr mkConst(BitVectorRepeat arg0);

    Expr mkConst(BitVectorExtract arg0);

    Expr mkConst(BitVectorRotateLeft arg0);

    Expr mkConst(BitVectorSignExtend arg0);

    Expr mkConst(BitVectorZeroExtend arg0);

    Expr mkConst(BitVectorRotateRight arg0);

    Expr mkConst(IntToBitVector arg0);

    Expr mkConst(UninterpretedConstant arg0);

    Expr mkConst(Kind arg0);

    Expr mkConst(DatatypeIndexConstant arg0);

    Expr mkConst(TupleUpdate arg0);

    Expr mkConst(RecordUpdate arg0);

    Expr mkConst(Rational arg0);

    Expr mkConst(BitVector arg0);

    Expr mkConst(EmptySet arg0);

    Expr mkConst(CVC4String arg0);

    Expr mkConst(TypeConstant arg0);

    Expr mkConst(boolean arg0);
}
