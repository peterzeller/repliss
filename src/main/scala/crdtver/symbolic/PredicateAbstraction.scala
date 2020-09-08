package crdtver.symbolic

import crdtver.language.TypedAst
import crdtver.language.TypedAst.{CallInfoType, IdType, InTypeDecl, InTypeExpr, InvocationInfoType, InvocationResultType, SimpleType}
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
    constraints += NamedConstraint("happensBefore_exists_l", 10, {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1 happensBefore c2)
          --> (c1.op !== SCallInfoNone())
      )
    })
    // happensBefore_in_calls_right
    constraints += NamedConstraint("happensBefore_exists_r", 10, {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (c1 happensBefore c2)
          --> (c2.op !== SCallInfoNone())
      )
    })

    // sequential happens-before relation between calls in the same invocation
    // state_wellFormed_same_invocation_sequential
    constraints += NamedConstraint("invocation_sequential", 20, {
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
    constraints += NamedConstraint("visibleCalls_exist", 10, {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c,
        c.isVisible
          --> (c.op !== SCallInfoNone())
      )
    })

    // visible calls forms consistent snapshot
    // transaction consistent
    // wf_transactionConsistent_noTx
    constraints += NamedConstraint("visibleCalls_transaction_consistent1", 10, {
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
    constraints += NamedConstraint("visibleCalls_causally_consistent", 10, {
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
    constraints += NamedConstraint("happensBefore_non_reflex", 10, {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c, !(c happensBefore c))
    })
    // happensBefore_transitive
    constraints += NamedConstraint("happensBefore_trans", 10, {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      val z = ctxt.makeBoundVariable[SortCallId]("z")
      forallL(List(x, y, z),
        ((x happensBefore y) && (y happensBefore z)) --> (x happensBefore z)
      )
    })
    // state_wellFormed_hb_antisym
    constraints += NamedConstraint("happensBefore_antisym", 10, {
      val x = ctxt.makeBoundVariable[SortCallId]("x")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      forallL(List(x, y),
        (x happensBefore y) --> !(y happensBefore x)
      )
    })


    // no invocation implies no result
    // state_wellFormed_invocation_before_result
    constraints += NamedConstraint("no_invocation_implies_no_result", 10, {
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val y = ctxt.makeBoundVariable[SortCallId]("y")
      forallL(List(i),
        (i.op === SInvocationInfoNone()) --> (i.res === SReturnValNone())
      )
    })

    // transaction consistency with happens before:
    // wf_transaction_consistent_l
    constraints += NamedConstraint("happens_before_transaction_consistent_l", 10, {
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
    // wf_transaction_consistent_r
    constraints += NamedConstraint("happens_before_transaction_consistent_r", 10, {
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
    constraints += NamedConstraint("WF_invocationCalls", 10, {
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
    constraints += NamedConstraint("WF_callOrigin", 10, {
      val c = ctxt.makeBoundVariable[SortCallId]("c")

      forall(c,
        c.tx.isNone <-->
          (c.op === SCallInfoNone()))
    })




    // when the transaction invocation is none, then there can be no calls in the transaction
    // state_wellFormed_transactionOrigin_callOrigin
    constraints += NamedConstraint("WF_transactionOrigin_callOrigin", 10, {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")
      forall(tx,
        tx.invocation.isNone -->
          forall(c, c.tx !== SSome(tx)))
    })


    // variation of happensBefore_in_calls_right
    constraints += NamedConstraint("WF_no_call_implies_no_happensBefore", 10, {
      val c = ctxt.makeBoundVariable[SortCallId]("c")

      forall(c,
        (c.op === SCallInfoNone()) -->
          (c.happensBeforeSet === SSetEmpty(SortCallId())))
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
    constraints += NamedConstraint("WF_callOrigin_exists", 10, {
      val ca = ctxt.makeBoundVariable[SortCallId]("ca")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(ca, forall(tx,
        (ca.tx === SSome(tx)) -->
          !tx.invocation.isNone))
    })


    // transactionOrigin exists
    // wf_no_invocation_no_origin
    constraints += NamedConstraint("WF_transactionOrigin_exists", 10, {
      val i = ctxt.makeBoundVariable[SortInvocationId]("i")
      val tx = ctxt.makeBoundVariable[SortTxId]("tx")

      forall(tx, forall(i,
        (tx.invocation === SSome(i)) -->
          (i.op !== SInvocationInfoNone())))
    })


    for (proc <- prog.procedures; idTypeDecl <- prog.idTypes) {
      val idType = idTypeDecl.toTypeExpr.asInstanceOf[IdType]

      // all unique ids in parameters of method invocations are known ids
      constraints += NamedConstraint(s"${proc.name.name}_${idType.name}_args_known", 10, {
        val i = ctxt.makeBoundVariable[SortInvocationId]("i")
        val uIds = ctxt.uniqueIds_func(SortInvocationInfo(), idType)
        forall(i, uIds(i.op).isSubsetOf(state.knownIds(idType)))
      })

      // all unique ids in returned values of method invocations are known ids
      constraints += NamedConstraint(s"${proc.name.name}_${idType.name}_res_known", 10, {
        val i = ctxt.makeBoundVariable[SortInvocationId]("i")
        val uIds = ctxt.uniqueIds_func(SortInvocationRes(), idType)
        forall(i, uIds(i.res).isSubsetOf(state.knownIds(idType)))
      })

    }

    // all unique ids in database calls are generated ids
    for (idTypeDecl <- prog.idTypes) {
      val idType = idTypeDecl.toTypeExpr.asInstanceOf[IdType]

      constraints += NamedConstraint(s"database_calls_${idType.name}_are_generated", 10, {
        val c = ctxt.makeBoundVariable[SortCallId]("c")
        val x = ctxt.makeBoundVariable("x")(translateType(idType))
        val uIds = ctxt.uniqueIds_func(SortCall(), idType)
        forallL(List(c, x), uIds(c.op).contains(x) --> (state.generatedIds(idType).get(x) !== SNone(implicitly)))
      })
    }


    // if an id is known it was generated
    // wf_onlyGeneratedIdsInKnownIds
    for ((t, knownIds) <- state.knownIds) {
      val x = ctxt.makeBoundVariable[SortCustomUninterpreted]("x")(SortCustomUninterpreted(t.name))
      constraints += NamedConstraint(s"${t.name}_knownIds_are_generated", 10,
        forall(x,
          knownIds.contains(x) -->
            !state.generatedIds(t).get(x).isNone)
      )
    }


    // snapshotAddition is a subset of all calls
    constraints += NamedConstraint("snapshot_addition_subset_calls", 10, {
      val c = ctxt.makeBoundVariable[SortCallId]("c")
      forall(c, state.snapshotAddition.contains(c) --> (c.op !== SCallInfoNone()))
    })
    // snapshotAddition is transaction consistent
    constraints += NamedConstraint("snapshot_addition_transaction_consistent", 10, {
      val c1 = ctxt.makeBoundVariable[SortCallId]("c1")
      val c2 = ctxt.makeBoundVariable[SortCallId]("c2")
      forallL(List(c1, c2),
        (state.snapshotAddition.contains(c1) && (c1.tx === c2.tx)) --> state.snapshotAddition.contains(c2))
    })
    // snapshotAddition is causally consistent
    constraints += NamedConstraint("snapshot_addition_transaction_consistent", 10, {
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
    // state_monotonicGrowth_callOrigin
    constraints += NamedConstraint("growth_callOrigin", 10, {
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
    // state_monotonicGrowth_calls2
    constraints += NamedConstraint("growth_calls", 10, {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.op(state2) === c.op(state)))
    })


    // monotonic growth of happensbefore
    // --> no new calls can be added before:
    // state_monotonicGrowth_happensBefore
    constraints += NamedConstraint("growth_happensbefore", 10, {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.happensBeforeSet(state2) === c.happensBeforeSet(state)))
    })

    // monotonic growth of call transaction
    // state_monotonicGrowth_callOrigin_unchanged
    constraints += NamedConstraint("growth_call_tx", 10, {
      val c = ctxt.makeVariable[SortCallId]("c")
      forall(c,
        (c.op(state) !== SCallInfoNone()) --> (c.tx(state2) === c.tx(state)))
    })


    // monotonic growth of transaction origin
    // state_monotonicGrowth_transactionOrigin
    constraints += NamedConstraint("growth_tx_origin", 10, {
      val tx = ctxt.makeVariable[SortTxId]("tx")
      forall(tx,
        !tx.invocation(state).isNone --> (tx.invocation(state2) === tx.invocation(state)))
    })


    // monotonic growth of invocations
    // state_monotonicGrowth_invocOp_unchanged
    constraints += NamedConstraint("growth_invocation_op", 10, {
      val i = ctxt.makeVariable[SortInvocationId]("i")
      forall(i,
        (i.op(state) !== SInvocationInfoNone()) --> (i.op(state2) === i.op(state)))
    })

    // monotonic growth of invocationResult
    // state_monotonicGrowth_invocRes_unchanged
    constraints += NamedConstraint("growth_invocation_res", 10, {
      val i = ctxt.makeVariable[SortInvocationId]("i")
      forall(i,
        (i.res(state) !== SReturnValNone()) --> (i.res(state2) === i.res(state)))
    })

    // no new calls added to existing transactions:
    // state_monotonicGrowth_no_new_calls_in_committed_transactions
    // (plus the fact that all transactions are always committed in single-invocation)
    constraints += NamedConstraint("old_transactions_unchanged", 10, {
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
    val func: UninterpretedFunction[SortSet[SortCustomUninterpreted]] = ctxt.uniqueIds_func(op.typ, t)
    SFunctionCall(func.returnType, func, List(op))
  }


  /**
   * Create implementations of unique id functions
   */
  def makeUniqueIdConstraints(implicit ctxt: SymbolicContext): List[NamedConstraint] = {

    val prog = ctxt.prog
    val res = new ListBuffer[NamedConstraint]
    // add a function for each custom type
    for (idTypeDecl <- prog.types; if idTypeDecl.isIdType) yield {
      val idType = IdType(idTypeDecl.name.name)()
      val idSort = translateType(idType)
      val idTypeSet = SortSet(idSort)

      val emptySet: SymbolicSet[SortCustomUninterpreted] = SSetEmpty(idSort)

      // for call
      def handleDataType(t: InTypeExpr): Unit = {
        val dtImpl = ctxt.translateSortDatatypeToImpl(t)
        val dt = ctxt.translateSortDatatype(t)
        for (c <- dtImpl.constructors.values) yield {
          val dtVars: List[SymbolicVariable[SymbolicSort]] =
            for (p <- c.args) yield
              ctxt.makeBoundVariable(p.name)(p.typ)
          val value: SVal[SymbolicSort] =
            SDatatypeValue(dtImpl, c.name, dtVars, dt).castUnsafe[SymbolicSort]

          val sets = for ((p, v) <- c.args.zip(dtVars)) yield {
            val f = ctxt.uniqueIds_func(p.typ, idType)
            f.apply(v)
          }
          val func = ctxt.uniqueIds_func(dt, idType)
          res += NamedConstraint(s"unique_ids_${t}_${c.name}_def", 10,
            forallL(dtVars, (func.apply(value) === unionL(sets)(idSort))))
        }
      }

      // some builtin types:
      handleDataType(CallInfoType())
      handleDataType(InvocationInfoType())
      handleDataType(InvocationResultType())


      for (typeDecl <- prog.types) yield {
        val t = typeDecl.toTypeExpr
        val sort = ExprTranslation.translateType(t)

        val func = ctxt.uniqueIds_func(sort, idType)

        if (typeDecl.isIdType) {
          val paramVar: SymbolicVariable[SymbolicSort] = ctxt.makeBoundVariable("x")(sort)
          val set =
            if (idType == t)
            // for the same type, it is the singleton set
              new SetSValExtensions(emptySet) + paramVar.cast(idSort)
            else
              emptySet

          res += NamedConstraint(s"${func.name}_def", 10,
            forall(paramVar, func.apply(paramVar) === set))
        } else if (typeDecl.dataTypeCases.nonEmpty) {
          handleDataType(t)
        } else {
          val paramVar: SymbolicVariable[SymbolicSort] = ctxt.makeBoundVariable("x")(sort)
          res += NamedConstraint(s"${func.name}_def", 10,
            forall(paramVar, func.apply(paramVar) === emptySet))
        }
      }



    }
    res.toList

  }


}
