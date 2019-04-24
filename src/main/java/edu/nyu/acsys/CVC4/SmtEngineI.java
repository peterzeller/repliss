package edu.nyu.acsys.CVC4;

public interface SmtEngineI {
    boolean isFullyInited();

    void setLogic(String logic);

    void setLogic(LogicInfo logic);

    LogicInfo getLogicInfo();

    void setInfo(String key, SExpr value);

    SExpr getInfo(String key);

    void setOption(String key, SExpr value);

    void setIsInternalSubsolver();

    void setFilename(String filename);

    String getFilename();

    SWIGTYPE_p_CVC4__Model getModel();

    Expr getSepHeapExpr();

    Expr getSepNilExpr();

    SExpr getOption(String key);

    void defineFunction(Expr func, vectorExpr formals, Expr formula);

    boolean isDefinedFunction(Expr func);

    void defineFunctionsRec(vectorExpr funcs, vectorVectorExpr formals, vectorExpr formulas);

    void defineFunctionRec(Expr func, vectorExpr formals, Expr formula);

    Result assertFormula(Expr e, boolean inUnsatCore);

    Result assertFormula(Expr e);

    Result query(Expr assumption, boolean inUnsatCore);

    Result query(Expr assumption);

    Result query();

    Result query(vectorExpr assumptions, boolean inUnsatCore);

    Result query(vectorExpr assumptions);

    Result checkSat(Expr assumption, boolean inUnsatCore);

    Result checkSat(Expr assumption);

    Result checkSat();

    Result checkSat(vectorExpr assumptions, boolean inUnsatCore);

    Result checkSat(vectorExpr assumptions);

    vectorExpr getUnsatAssumptions();

    void declareSygusVar(String id, Expr var, Type type);

    void declareSygusPrimedVar(String id, Type type);

    void declareSygusFunctionVar(String id, Expr var, Type type);

    void declareSynthFun(String id, Expr func, Type type, boolean isInv, vectorExpr vars);

    void assertSygusConstraint(Expr constraint);

    void assertSygusInvConstraint(Expr inv, Expr pre, Expr trans, Expr post);

    Result checkSynth();

    Expr simplify(Expr e);

    Expr expandDefinitions(Expr e);

    Expr getValue(Expr e);

    boolean addToAssignment(Expr e);

    SWIGTYPE_p_std__vectorT_std__pairT_CVC4__Expr_CVC4__Expr_t_t getAssignment();

    Proof getProof();

    void printInstantiations(java.io.OutputStream out);

    void printSynthSolution(java.io.OutputStream out);

    void getSynthSolutions(SWIGTYPE_p_std__mapT_CVC4__Expr_CVC4__Expr_t sol_map);

    Expr doQuantifierElimination(Expr e, boolean doFull, boolean strict);

    Expr doQuantifierElimination(Expr e, boolean doFull);

    void getInstantiatedQuantifiedFormulas(vectorExpr qs);

    void getInstantiations(Expr q, vectorExpr insts);

    void getInstantiationTermVectors(Expr q, vectorVectorExpr tvecs);

    UnsatCore getUnsatCore();

    vectorExpr getAssertions();

    void push();

    void pop();

    void reset();

    void resetAssertions();

    void interrupt();

    void setResourceLimit(long units, boolean cumulative);

    void setResourceLimit(long units);

    void setTimeLimit(long millis, boolean cumulative);

    void setTimeLimit(long millis);

    long getResourceUsage();

    long getTimeUsage();

    long getResourceRemaining();

    long getTimeRemaining();

    ExprManager getExprManager();

    Statistics getStatistics();

    SExpr getStatistic(String name);

    void safeFlushStatistics(int fd);

    Result getStatusOfLastCommand();

    void setUserAttribute(String attr, Expr expr, vectorExpr expr_values, String str_value);

    void setPrintFuncInModel(Expr f, boolean p);

    void beforeSearch();

    SWIGTYPE_p_LemmaChannels channels();

    void setReplayStream(ExprStream exprStream);

    boolean getExpressionName(Expr e, SWIGTYPE_p_std__string name);

    void setExpressionName(Expr e, String name);
}
