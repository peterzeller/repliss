package crdtver.symbolic

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{IdType, InTypeDecl, SimpleType}
import crdtver.symbolic.ExprTranslation.translateType
import crdtver.symbolic.SVal.{SetSValExtensions, SymbolicSet, and, exists, forall, forallL, unionL}
import crdtver.symbolic.SymbolicEvaluator.{makeGeneratedIdsVar, makeKnownIdsVar}
import crdtver.symbolic.SymbolicMapVar.symbolicMapVar
import crdtver.utils.MapUtils.MapExtensions

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Predicate abstractions for wellformed predicate
 */
object PredicateAbstraction {
  /** *
   * Adds assumptions that the given state is well-formed.
   */
  def assumeWellformed(where: String, state: SymbolicState, ctxt: SymbolicContext): Iterable[NamedConstraint] = {
    val prog = ctxt.prog
    val constraints = mutable.ListBuffer[NamedConstraint]()

    implicit val implicitState: SymbolicState = state

    // no happensBefore relation between non-existing calls
    // happensBefore_in_calls_left
    constraints += NamedConstraint("happensBefore_exists_l", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1 happensBefore c2)
          --> (c1.op !== SCallInfoNone())
      )
    })
    // happensBefore_in_calls_right
    constraints += NamedConstraint("happensBefore_exists_r", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1 happensBefore c2)
          --> (c2.op !== SCallInfoNone())
      )
    })

    // sequential happens-before relation between calls in the same invocation
    // state_wellFormed_same_invocation_sequential
    constraints += NamedConstraint("invocation_sequential", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      val tx1 = ctxt.makeBoundVariable[SortTxId]("tx1")
      val tx2 = ctxt.makeBoundVariable[SortTxId]("tx2")
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      forallL(List(c1, tx1, i, c2, tx2),
        ((c1.tx === SSome(tx1))
          && (tx1.invocation === SSome(i))
          && (c2.tx === SSome(tx2))
          && (tx2.invocation === SSome(i))
          && (c1 !== c2))
          --> ((c1 happensBefore c2) || (c2 happensBefore c1))
      )
    })


    // visible calls are a subset of all calls
    // state_wellFormed_vis_subset_calls
    constraints += NamedConstraint("visibleCalls_exist", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c,
        c.isVisible
          --> (c.op !== SCallInfoNone())
      )
    })

    // visible calls forms consistent snapshot
    // transaction consistent
    // wf_transactionConsistent_noTx
    constraints += NamedConstraint("visibleCalls_transaction_consistent1", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1.isVisible
          && (c1 inSameTransactionAs c2)
          && (c2.op !== SCallInfoNone()))
          --> c2.isVisible
      )
    })

    // wf_causallyConsistent1
    constraints += NamedConstraint("visibleCalls_causally_consistent", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c2.isVisible
          && (c1 happensBefore c2))
          --> c1.isVisible
      )
    })


    // happensBefore is a strict partial order (transitivity, antisymmetric)
    // happensBefore_irrefl
    constraints += NamedConstraint("happensBefore_non_reflex", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c, !(c happensBefore c))
    })
    // wellFormed_state_causality
    constraints += NamedConstraint("happensBefore_trans", {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      val z = ctxt.makeBoundVariable[SortCallId]("z")
      forallL(List(x, y, z),
        ((x happensBefore y) && (y happensBefore z)) --> (x happensBefore z)
      )
    })
    // state_wellFormed_hb_antisym
    constraints += NamedConstraint("happensBefore_antisym", {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      forallL(List(x, y),
        (x happensBefore y) --> !(y happensBefore x)
      )
    })


    // no invocation implies no result
    // state_wellFormed_invocation_before_result
    constraints += NamedConstraint("no_invocation_implies_no_result", {
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      forallL(List(i),
        (i.op === SInvocationInfoNone()) --> (i.res === SReturnValNone())
      )
    })

    // transaction consistency with happens before:
    // wellFormed_state_transaction_consistent
    constraints += NamedConstraint("happens_before_transaction_consistent_l", {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y1 = ctxt.makeBoundVariable[SortCallId]("y1")
      val y2 = ctxt.makeBoundVariable[SortCallId]("y2")
      forallL(List(x, y1, y2),
        (((y1 inSameTransactionAs y2)
          && !(x inSameTransactionAs y1)
          && (y1 happensBefore x))
          --> (y2 happensBefore x))
      )
    })
    // wellFormed_state_transaction_consistent
    constraints += NamedConstraint("happens_before_transaction_consistent_r", {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y1 = ctxt.makeBoundVariable[SortCallId]("y1")
      val y2 = ctxt.makeBoundVariable[SortCallId]("y2")
      forallL(List(x, y1, y2),
        ((y1 inSameTransactionAs y2)
          && !(x inSameTransactionAs y1)
          && (x happensBefore y1))
          --> (x happensBefore y2)
      )
    })



    // TODO not needed because of different encoding?:
    // invocation happens-before of origins implies happens-before of calls
    // no result implies not in invocation happens before
    // in happens before implies not NoResult


    // old ....


    // definition of invocationCalls
    constraints += NamedConstraint("WF_invocationCalls", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(i, forall(c,
        i.calls.contains(c) <-->
          exists(tx,
            (c.tx === SSome(tx))
              && (tx.invocation === SSome(i)))))
    })



    // domain calls = domain callsOrigin
    // wellFormed_callOrigin_dom3
    constraints += NamedConstraint("WF_callOrigin", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")

      forall(c,
        c.tx.isNone <-->
          (c.op === SCallInfoNone()))
    })




    // when the transaction invocation is none, then there can be no calls in the transaction
    // state_wellFormed_transactionOrigin_callOrigin
    constraints += NamedConstraint("WF_transactionOrigin_callOrigin", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")
      forall(tx,
        tx.invocation.isNone -->
          forall(c, c.tx !== SSome(tx)))
    })


    constraints += NamedConstraint("WF_no_call_implies_no_happensBefore", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")

      forall(c,
        (c.op === SCallInfoNone()) -->
          (c.happensBeforeSet === SSetEmpty[SortCallId]()))
    })

    //    see wellFormed_happensBefore_calls_l
    //    constraints += NamedConstraint("WF_no_call_implies_not_in_happensBefore", {
    //      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
    //      val cb = ctxt.makeBoundVariable[SortCallId]("cb")
    //
    //      forall(ca, forall(cb,
    //        ca.tx.isNone -->
    //          !(ca happensBefore cb)))
    //    })

    // callOrigin exists
    // state_wellFormed_transactionOrigin_callOrigin
    constraints += NamedConstraint("WF_callOrigin_exists", {
      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(ca, forall(tx,
        (ca.tx === SSome(tx)) -->
          !tx.invocation.isNone))
    })


    // transactionOrigin exists
    // wf_no_invocation_no_origin
    constraints += NamedConstraint("WF_transactionOrigin_exists", {
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(tx, forall(i,
        (tx.invocation === SSome(i)) -->
          (i.op !== SInvocationInfoNone())))
    })



    // all parameters of method invocations are known ids
    for (proc <- prog.procedures) {
      for ((arg, argI) <- proc.params.zipWithIndex) {
        arg.typ match {
          case t: IdType =>
            val i = ctxt.makeBoundVariable[SortInvocationId]("i")
            val argVariables: List[SymbolicVariable[SortValue]] = proc.params.map(p => ctxt.makeBoundVariable[SortValue](p.name.name)(ExprTranslation.translateType(p.typ)(ctxt).asInstanceOf[SortValue]))
            val knownIds: SVal[SortSet[SortCustomUninterpreted]] = state.knownIds.getE(t)
            constraints += NamedConstraint(s"${proc.name.name}_parameter_${arg.name}_known",
              forallL(i :: argVariables,
                (i.op === SInvocationInfo(proc.name.name, argVariables)) -->
                  knownIds.contains(argVariables(argI).asInstanceOf[SVal[SortCustomUninterpreted]]))
            )
          case _ =>
          // should also handle nested ids
        }
      }
    }



    // all returned values of method invocations are known ids
    for (proc <- prog.procedures) {
      proc.returnType match {
        case t: IdType =>
          val i = ctxt.makeBoundVariable[SortInvocationId]("i")
          val r = ctxt.makeBoundVariable("result")(translateType(t))
          val knownIds: SVal[SortSet[SortCustomUninterpreted]] = state.knownIds(t)
          constraints += NamedConstraint(s"${proc.name.name}_result_known",
            forallL(List(i, r),
              (i.res === SReturnVal(proc.name.name, r.upcast)) -->
                knownIds.contains(r))
          )
        case _ =>
        // TODO should also handle nested ids
      }
    }

    // all parameters of database calls are generated
    {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      for (idType <- prog.idTypes) {
        val iType = idTypeExpr(idType)
        val iGenerated = state.generatedIds(iType)
        val uid = ctxt.makeBoundVariable[SortCustomUninterpreted]("uid")(translateType(iType))
        constraints += NamedConstraint("call_parameters_generated",
          forall(c, forall(uid,
            uniqueIds_op(c.op, iType)(ctxt).contains(uid) --> iGenerated.get(uid).isDefined)))
      }
    }

    // TODO go through all datatypes and add uniqueIds_op constraints
    {

    }



    // if an id is known it was generated
    for ((t, knownIds) <- state.knownIds) {
      val x = ctxt.makeBoundVariable[SortCustomUninterpreted]("x")(SortCustomUninterpreted(t.name))
      constraints += NamedConstraint(s"${t.name}_knownIds_are_generated",
        forall(x,
          knownIds.contains(x) -->
            !state.generatedIds(t).get(x).isNone)
      )
    }


    // snapshotAddition is a subset of all calls
    constraints += NamedConstraint("snapshot_addition_subset_calls", {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c, state.snapshotAddition.contains(c) --> (c.op !== SCallInfoNone()))
    })
    // snapshotAddition is transaction consistent
    constraints += NamedConstraint("snapshot_addition_transaction_consistent", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (state.snapshotAddition.contains(c1) && (c1.tx === c2.tx)) --> state.snapshotAddition.contains(c2))
    })
    // snapshotAddition is causally consistent
    constraints += NamedConstraint("snapshot_addition_transaction_consistent", {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (state.snapshotAddition.contains(c1) && (c2 happensBefore c1)) --> state.snapshotAddition.contains(c2))
    })


    // TODO compare with WhyTranslation.wellformedConditions

    constraints.map(c => c.copy(description = s"${where}_${c.description}")).toList
  }


  def monotonicGrowth(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    implicit val ictxt: SymbolicContext = ctxt
    // create new variables for new state
    val state2 = state.copy(
      calls = symbolicMapVar("calls"),
      happensBefore = symbolicMapVar("happensBefore"),
      callOrigin = symbolicMapVar("callOrigin"),
      transactionOrigin = symbolicMapVar("transactionOrigin"),
      invocationCalls = symbolicMapVar("invocationCalls"),
      invocationOp = symbolicMapVar("invocationOp"),
      invocationRes = symbolicMapVar("invocationRes"),
      generatedIds = makeGeneratedIdsVar,
      knownIds = makeKnownIdsVar,
      snapshotAddition = SSetVar(ctxt.makeVariable("snapshotAddition"))
    )
    val constraints = mutable.ListBuffer[NamedConstraint]()


    // call origin growths:
    constraints += NamedConstraint("growth_callOrigin", {
      val c = ctxt.makeVariable[SortCallId]("c")
      val tx = ctxt.makeVariable[SortTxId]("tx")
      forall(c, forall(tx,
        (c.tx(state) === SSome(tx))
          --> (c.tx(state2) === SSome(tx))))
    })

    // monotonic growth of visible calls
    //    constraints += NamedConstraint("growth_visible_calls", {
    //      val c = ctxt.makeVariable[SortCallId]("c")
    //      forall(c,
    //        c.isVisible(state) --> c.isVisible(state2))
    //    })


    // monotonic growth of call ops
    constraints += NamedConstraint("growth_calls", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.op(state2) === c.op(state)))
    })


    // monotonic growth of happensbefore
    // --> no new calls can be added before:
    constraints += NamedConstraint("growth_happensbefore", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.happensBeforeSet(state2) === c.happensBeforeSet(state)))
    })

    // monotonic growth of call transaction
    constraints += NamedConstraint("growth_call_tx", {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.tx(state2) === c.tx(state)))
    })


    // monotonic growth of transaction origin
    constraints += NamedConstraint("growth_tx_origin", {
      val tx = ctxt.makeVariable[SortTxId]("tx")
      forall(tx,
        !tx.invocation(state).isNone --> (tx.invocation(state2) === tx.invocation(state)))
    })


    // monotonic growth of invocations
    constraints += NamedConstraint("growth_invocation_op", {
      val i = ctxt.makeVariable[SortInvocationId]("i")
      forall(i,
        (i.op(state) !== SInvocationInfoNone()) --> (i.op(state2) === i.op(state)))
    })

    // monotonic growth of invocationResult
    constraints += NamedConstraint("growth_invocation_res", {
      val i = ctxt.makeVariable[SortInvocationId]("i")
      forall(i,
        (i.res(state) !== SReturnValNone()) --> (i.res(state2) === i.res(state)))
    })

    // no new calls added to existing transactions:
    constraints += NamedConstraint("old_transactions_unchanged", {
      val tx = ctxt.makeVariable[SortTxId]("tx")
      val c = ctxt.makeVariable[SortCallId]("c")
      forallL(List(c, tx),
        ((c.op(state) === SCallInfoNone())
          && (c.op(state2) !== SCallInfoNone())
          && (c.tx(state2) === SSome(tx)))
          --> tx.invocation(state).isNone)
    })


    state2.withConstraints(constraints)
  }


  def idTypeExpr(idType: InTypeDecl): IdType = {
    require(idType.isIdType)
    IdType(idType.name.name)(idType.source)
  }

  /** function that returns the unique identifiers of type t in an operation */
  def uniqueIds_op[T <: SymbolicSort](op: SVal[T], t: IdType)(implicit ctxt: SymbolicContext): SVal[SortSet[SortCustomUninterpreted]] = {
    val func: UninterpretedFunction = ctxt.uniqueIds_func(op.typ, t)
    SFunctionCall(func.returnType.asInstanceOf[SortSet[SortCustomUninterpreted]], func, List(op))
  }


  /**
   * Create implementations of unique id functions
   */
  def makeUniqueIdConstraints(implicit ctxt: SymbolicContext): List[NamedConstraint] = {

    val prog = ctxt.prog
    // add a function for each custom type
    val res: List[List[NamedConstraint]] = for (idTypeDecl <- prog.types; if idTypeDecl.isIdType) yield {
      val idType = IdType(idTypeDecl.name.name)()
      val idSort = translateType(idType)
      val idTypeSet = SortSet(idSort)

      for (typeDecl <- prog.types) yield {
        val t = typeDecl.toTypeExpr
        val sort = ExprTranslation.translateType(t)

        val func = ctxt.uniqueIds_func(sort, idType)

        val paramVar: SymbolicVariable[SymbolicSort] = ctxt.makeBoundVariable("x")(sort)

        val emptySet: SymbolicSet[SortCustomUninterpreted] = SSetEmpty()(idSort)
        val fdef: SVal[SortBoolean] =
          if (typeDecl.isIdType) {
            var set = emptySet
            if (idType == t)
            // for the same type, it is the singleton set
              set = new SetSValExtensions(set) + paramVar.cast(idSort)

            forall(paramVar, func.apply(paramVar)(idTypeSet) === set)
          } else if (typeDecl.dataTypeCases.nonEmpty) {
            val dtImpl = ctxt.translateSortDatatypeToImpl(t)
            val dt = ctxt.translateSortDatatype(t)
            val cases: List[SVal[SortBoolean]] =
              for (c <- typeDecl.dataTypeCases) yield {
                val dtVars: List[SymbolicVariable[SymbolicSort]] =
                  for (p <- c.params) yield
                    ctxt.makeBoundVariable(p.name.name)(translateType(p.typ))
                val value: SVal[SymbolicSort] =
                  SDatatypeValue(dtImpl, c.name.name, dtVars, dt).castUnsafe[SymbolicSort]

                val sets = for ((p, v) <- c.params.zip(dtVars)) yield {
                  p.typ match {
                    case _: SimpleType | _: IdType =>
                      val s = ctxt.translateSort(p.typ)
                      val f = ctxt.uniqueIds_func(s, idType)
                      f.apply(v)(idTypeSet)
                    case _ =>
                      emptySet
                  }
                }
                forallL(dtVars, (func.apply(value)(idTypeSet) === unionL(sets)(idSort)))
              }
            and(cases)
          } else {
            forall(paramVar, func.apply(paramVar)(idTypeSet) === emptySet)
          }

        val r = NamedConstraint(s"${func.name}_def", fdef)
        println(r)
        r
      }
    }
    res.flatten

  }


}
