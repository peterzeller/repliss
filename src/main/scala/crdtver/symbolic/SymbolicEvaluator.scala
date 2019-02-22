package crdtver.symbolic

import crdtver.Repliss.QuickcheckCounterexample
import crdtver.language.InputAst
import crdtver.language.InputAst.{IdType, InProgram, InTypeExpr}
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicMapVar.symbolicMapVar
import crdtver.symbolic.SymbolicSort._

class SymbolicExecutionError(msg: String) extends RuntimeException(msg)

class SymbolicEvaluator(
  val prog: InProgram
) {


  def checkProgram(): Map[String, SymbolicExecutionError] = {
    println("checking program")
    val counterExamples =
      for {
        proc <- prog.procedures
        counterExample <- checkProcedure(proc)
      } yield {
        proc.name.name -> counterExample
      }
    counterExamples.toMap
  }

  private def idTypes(): List[InputAst.InTypeDecl] =
    prog.types.filter(_.isIdType)




  private def checkProcedure(proc: InputAst.InProcedure): Option[SymbolicExecutionError] = {
    try {
      println(s"checking procedure ${proc.name}")
      val z3Translation = new Z3Translation()
      implicit val ctxt: SymbolicContext = new SymbolicContext(z3Translation, proc.name.name, prog)
      z3Translation.symbolicContext = ctxt

      val params = makeVariablesForParameters(ctxt, proc.params)

      val generatedIds: Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] =
        generatedIdsVar(ctxt)
      val knownIds: Map[IdType, SVal[SortSet[SortCustomUninterpreted]]] =
        knownIdsVar(ctxt)


      // in the beginning everything is unknown so we use symbolic variables:
      val state: SymbolicState = SymbolicState(
        calls = symbolicMapVar("calls"),
        //[SortCallId, SortOption[SortCall], Nothing](ctxt.makeVariable("calls"))(default()),
        happensBefore = symbolicMapVar("happensBefore"),
        callOrigin = symbolicMapVar("callOrigin"),
        transactionOrigin = symbolicMapVar("transactionOrigin"),
        transactionStatus = symbolicMapVar("transactionStatus"),
        generatedIds = generatedIds,
        knownIds = knownIds,
        invocationCalls = symbolicMapVar("invocationCalls"),
        invocationOp = symbolicMapVar("invocationOp"),
        invocationRes = symbolicMapVar("invocationRes"),
        currentInvocation = ctxt.makeVariable("currentInvocation"),
        localState = params.toMap,
        visibleCalls = SSetEmpty()
      )

      // there are a few restrictions we can assume for the initial state:
      // this follows the begin-invoc rule

      // >> invocationOp S i = None;
      ctxt.addConstraint(SMapGet(state.invocationOp, state.currentInvocation) === SInvocationInfoNone())

      // >> procedure (prog S) procName args ≜ (initState, impl);
      // >>   uniqueIdsInList args ⊆ knownIds S';
      // >>   state_wellFormed S';
      // >>   ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
      val var_tx = ctxt.makeVariable[SortTxId]("tx")

      ctxt.addConstraint(forall(var_tx, state.transactionStatus(var_tx) !== SSome(SUncommitted())))

      // >>   invariant_all S';
      ctxt.addConstraint(invariant(state)(ctxt))
      // >>   invocationOp S' i = None;
      val i = ctxt.makeVariable[SortInvocationId]("i")

      // >>   prog S' = prog S;
      // >>   S'' = (S'⦇localState := (localState S')(i ↦ initState),
      // this is handled by makeVariablesForParameters
      // >>   currentProc := (currentProc S')(i ↦ impl),
      // see proc
      // >>   visibleCalls := (visibleCalls S')(i ↦ {}),
      // see state.visibleCalls
      // >>   invocationOp := (invocationOp S')(i ↦ (procName, args)) ⦈);
      // >>   ⋀tx. transactionOrigin S'' tx ≠ Some i
      // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
      val invocationInfo: SVal[SortInvocationInfo] = SInvocationInfo(proc.name.name, params.map(_._2))

      val state2 = state.copy(
        invocationOp = state.invocationOp.put(i, invocationInfo)
        //SymbolicMapUpdated(i, invocationInfo, state.invocationOp)
      )

      // check the invariant in state2:
      checkInvariant(ctxt, state2, s"directly after invocation of ${proc.name}") match {
        case Some(msg) =>
          throw new SymbolicExecutionError(msg)
        case None =>
        // ok
      }

      // continue evaluating the procedure body:
      executeStatement(proc.body, state2, ctxt, (s, _) => s)
      None
    } catch {
      case e: SymbolicExecutionError =>
        Some(e)
    }


  }


  private def knownIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicVariable[SortSet[SortCustomUninterpreted]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val sort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> ctxt.makeVariable[SortSet[SortCustomUninterpreted]](s"knownIds_${t.name}")(SortSet(sort))
    })
      .toMap
  }

  private def generatedIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val keySort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> symbolicMapVar[SortCustomUninterpreted, SortOption[SortInvocationId]](s"generatedIds_${t.name}")(keySort, implicitly, implicitly)
    })
      .toMap
  }

  def executeStatements(stmts: List[InputAst.InStatement], state: SymbolicState, ctxt: SymbolicContext, follow: (SymbolicState, SymbolicContext) => SymbolicState): SymbolicState = stmts match {
    case Nil =>
      follow(state, ctxt)
    case x :: xs =>
      executeStatement(x, state, ctxt, executeStatements(xs, _, _, follow))
  }


  /**
    * Asserts that the state is wellformed
    */
  def wellFormed(state: SymbolicState): SVal[SortBoolean] =
    SBool(true)

  def debugPrint(str: => String): Unit = {
    println(str)
  }

  private def executeStatement(stmt: InputAst.InStatement, state: SymbolicState, ctxt: SymbolicContext, follow: (SymbolicState, SymbolicContext) => SymbolicState): SymbolicState = {
    implicit val istate: SymbolicState = state
    stmt match {
      case InputAst.BlockStmt(source, stmts) =>
        debugPrint(s"Executing block in line ${source.getLine}")
        executeStatements(stmts, state, ctxt, follow)
      case InputAst.Atomic(source, body) =>
        debugPrint(s"Executing begin-atomic in line ${source.getLine}")
        val state2 = executeBeginAtomic(state, ctxt)
        executeStatement(body, state2, ctxt, (state3, ctxt) => {
          debugPrint(s"Executing end-atomic in line ${source.range.stop.line}")
          val state4 = executeEndAtomic(state3, ctxt)

          // assume state 4 wellformed
          ctxt.addConstraint(wellFormed(state4))
          // check invariant in state4
          checkInvariant(ctxt, state4, s"When committing transaction of atomic block in line ${source.getLine}.")

          follow(state4, ctxt)
        })
      case InputAst.LocalVar(source, variable) =>
        debugPrint(s"Executing local variable $variable in line ${source.getLine}")
        // nothing to do
        state
      case InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
        debugPrint(s"Executing if-statement in line ${source.getLine}")
        val condV: SVal[SortBoolean] = ExprTranslation.translate(cond)(bool, ctxt, state)
        ctxt.inContext(() => {
          // first assume the condition is true
          ctxt.addConstraint(condV)
          ctxt.check() match {
            case Unsatisfiable =>
            // then-branch cannot be taken
            case Unknown | _: Satisfiable =>
              debugPrint(s"Executing then-statement in line ${thenStmt.getSource().getLine}")
              executeStatement(thenStmt, state, ctxt, follow)
          }
        })
        // next assume the condition is false:
        debugPrint(s"Executing else-statement in line ${elseStmt.getSource().getLine}")
        ctxt.addConstraint(SNot(condV))
        ctxt.check() match {
          case Unsatisfiable =>
            // else-branch cannot be taken
            state.copy(satisfiable = false)
          case Unknown | _: Satisfiable =>
            executeStatement(elseStmt, state, ctxt, follow)
        }
      case InputAst.MatchStmt(source, expr, cases) =>
        // TODO
        ???
      case InputAst.CrdtCall(source, call) =>
        debugPrint(s"Executing CRDT call in line ${source.getLine}")
        val t: SVal[SortTxId] = state.currentTransaction.get
        val c: SVal[SortCallId] = ctxt.makeVariable("c" + state.currentCallIds.size)
        // assume new call c is distinct from all others:
        ctxt.addConstraint(SDistinct(c :: state.currentCallIds))
        ctxt.addConstraint(SEq(state.calls.get(c), SCallInfoNone()))

        // TODO maybe choose type based on query type
        // TODO assume query specification for res

        val args: List[SVal[SymbolicSort]] = call.args.map(a => ExprTranslation.translateUntyped(a)(ctxt, state))
        val callInfo: SVal[SortCall] = SCallInfo(call.functionName.name, args)

        val state2 = state.copy(
          calls = state.calls.put(c, callInfo),
          callOrigin = state.callOrigin.put(c, SSome(t)),
          visibleCalls = SSetInsert(state.visibleCalls, c),
          happensBefore = state.happensBefore.put(c, state.visibleCalls),
          invocationCalls = state.invocationCalls.put(state.currentInvocation,
            SSetInsert(SSetVar(state.invocationCalls.get(state.currentInvocation)), c))
        )

        follow(state2, ctxt)
      case InputAst.Assignment(source, varname, expr) =>
        debugPrint(s"Executing assignment in line ${source.getLine}")
        // use a new variable here to avoid duplication of expressions
        val v = ctxt.makeVariable(varname.name)(ctxt.translateSortVal(expr.getTyp))
        ctxt.addConstraint(v === ctxt.translateExprV(expr))
        val state2 = state.copy(localState = state.localState + (ProgramVariable(varname.name) -> v))
        follow(state2, ctxt)
      case InputAst.NewIdStmt(source, varname, typename) =>
        debugPrint(s"Executing new-id statement in line ${source.getLine}")
        val idType = typename.asInstanceOf[IdType]
        val vname = varname.name
        val newV: SVal[SortCustomUninterpreted] = ctxt.makeVariable(vname)(ctxt.translateSort( typename)).asInstanceOf[SVal[SortCustomUninterpreted]]
        ctxt.addConstraint(state.generatedIds(idType)(newV) === SNone(SortInvocationId()))
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(vname) -> newV)
        )
        follow(state2, ctxt)
      case InputAst.ReturnStmt(source, expr, assertions) =>
        debugPrint(s"Executing return statement in line ${source.getLine}")
        val returnv: SVal[SortValue] = ctxt.translateExprV(expr)

        val state2 = state.copy(
          invocationRes = state.invocationRes.put(state.currentInvocation, SReturnVal(ctxt.currentProcedure, returnv))
          // TODO update knownIds
        )
        follow(state2, ctxt)
      case InputAst.AssertStmt(source, expr) =>
        debugPrint(s"Executing assert statement in line ${source.getLine}")
        ctxt.inContext(() => {
          ctxt.addConstraint(SNot(ctxt.translateExpr(expr)))
          ctxt.check() match {
            case SymbolicContext.Unsatisfiable =>
            // check ok
            case SymbolicContext.Unknown =>
              throw new SymbolicExecutionError(s"Assertion in line ${source.getLine} might not hold.")
            case s: Satisfiable =>
              val model = ctxt.getModel(s)
              val localState =
                for ((v, x) <- state.localState) yield {
                  val str = model.executeForString(x.asInstanceOf[SVal[_ <: SymbolicSort]])
                  s"${v.name} -> $str"
                }
              throw new SymbolicExecutionError(
                s"""
                   |Assertion in line ${source.getLine} failed: $expr
                   |local variables:
                   |  ${localState.mkString("\n  ")}
                   |model:
                   |$model
                 """.stripMargin)
          }
        })
        follow(state, ctxt)
    }
  }

  def executeBeginAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.currentTransaction match {
      case Some(tx) =>
        throw new SymbolicExecutionError(s"Already in a transaction")
      case None =>
        // create variable for the new transaction
        val tx = ctxt.makeVariable[SortTxId]("tx")
        // transactionStatus S t = None;
        ctxt.addConstraint(SEq(SMapGet(state.transactionStatus, tx), SNone(SortTransactionStatus())))
        // state_monotonicGrowth i S S'
        val state2 = monotonicGrowth(state, ctxt)
        // ⋀t. transactionOrigin S t ≜ i ⟷ transactionOrigin S' t ≜ i; ― ‹No new transactions are added to current invocId.›
        val t = ctxt.makeVariable[SortTxId]("t")
        ctxt.addConstraint(
          forall(t, (state.transactionOrigin.get(t) === SSome(state.currentInvocation))
            === (state2.transactionOrigin.get(t) === SSome(state.currentInvocation)))
        )
        // no new calls are added to current invocation:
        ctxt.addConstraint(SEq(
          state.invocationCalls.get(state.currentInvocation),
          state2.invocationCalls.get(state.currentInvocation)))
        // invariant_all S';
        ctxt.addConstraint(invariant(state2)(ctxt))
        // ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
        val tx2 = ctxt.makeVariable[SortTxId]("tx2")
        ctxt.addConstraint(
          forall(tx2, state2.transactionStatus.get(tx2) !== SSome(SUncommitted()))
        )
        // newTxns ⊆ dom (transactionStatus S');
        val newTxns = SSetVar[SortTxId](ctxt.makeVariable("newTxns"))
        ctxt.addConstraint(
          newTxns.isSubsetOf(MapDomain(state2.transactionStatus))
        )


        // newCalls = callsInTransaction S' newTxns ↓ happensBefore S'
        val newCalls = SSetVar[SortCallId](ctxt.makeVariable("newCalls"))
        // TODO add restrictions to newCalls
        // vis' = vis ∪ newCalls
        val vis2 = SSetVar(ctxt.makeVariable[SortSet[SortCallId]]("vis"))
        ctxt.addConstraint(SEq(vis2, SSetUnion(state2.visibleCalls, newCalls)))
        // TODO ⋀c. callOrigin S' c ≠ Some t


        state2.copy(
          transactionStatus = state2.transactionStatus.put(tx, SSome(SUncommitted())),
          transactionOrigin = state2.transactionOrigin.put(tx, SSome(state2.currentInvocation)),
          currentTransaction = Some(tx),
          visibleCalls = vis2
        )
    }
  }

  def monotonicGrowth(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    implicit val ictxt = ctxt
    // create new variables for new state
    val state2 = state.copy(
      calls = symbolicMapVar("calls"),
      happensBefore = symbolicMapVar("happensBefore"),
      callOrigin = symbolicMapVar("callOrigin"),
      transactionOrigin = symbolicMapVar("transactionOrigin"),
      transactionStatus = symbolicMapVar("transactionStatus"),
      invocationCalls = symbolicMapVar("invocationCalls"),
      generatedIds = generatedIdsVar,
      knownIds = knownIdsVar
    )

    // TODO add constraints between old and new state

    state2
  }

  def executeEndAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.copy(
      currentTransaction = None,
      transactionStatus = state.transactionStatus.put(state.currentTransaction.get, SSome(SCommitted()))
    )
  }


  /** Checks the invariant in the given state.
    * Returns None if no problem was found and an error message otherwise.
    *
    * Implementation details:
    * The check is done by adding the negated invariant to the current context and checking if
    * the resulting context is unsatisfiable.
    *
    * If the context is satisfiable this means that there is a model such that all assumptions
    * in the current context are true, but the invariant is false.
    *
    * */
  private def checkInvariant(ctxt: SymbolicContext, state: SymbolicState, where: String): Option[String] = {

    println("checkInvariant: before")
    ctxt.check() match {
      case Unsatisfiable =>
        // ok
        throw new RuntimeException("failed before invariant-check ...")
      case Unknown =>
        println("checkInvariant1: unknown ... ")
//        throw new RuntimeException(s"")
      case s: Satisfiable =>
        println("checkInvariant1: Satisfiable, computing model ... ")
        val model = ctxt.getModel(s)
        Some(
          s"""Before invariant check it is consistent with " +
             |Model = $model
                    """.stripMargin)
    }

    ctxt.inContext(() => {
      ctxt.addConstraint(SNot(invariant(state)(ctxt)))
      ctxt.check() match {
        case Unsatisfiable =>
          println("checkInvariant: unsat, ok")
          // ok
          None
        case Unknown =>
          println("checkInvariant: unknown")
          Some(s"Could not prove invariant $where")
        case s: Satisfiable =>
          println("checkInvariant: satisfiable, computing model ...")
          val model = ctxt.getModel(s)
          Some(
            s"""Invariant does not hold $where" +
               |Model = $model
                """.stripMargin)
      }
    })
  }

  private def invariant(state: SymbolicState)(implicit ctxt: SymbolicContext): SVal[SortBoolean] = {
    val invExprs: List[SVal[SortBoolean]] = for (inv <- prog.invariants) yield {
      ExprTranslation.translate(inv.expr)(SymbolicSort.bool, ctxt, state)
    }
    SVal.and(invExprs)
  }


  private def makeVariablesForParameters(ctxt: SymbolicContext, params: List[InputAst.InVariable]): List[(ProgramVariable, SVal[SortValue])] = {
    for (p <- params) yield
      ProgramVariable(p.name.name) -> ctxt.makeVariable(p.name + "_init")(ctxt.translateSortVal(p.typ))
  }


}
