package crdtver.symbolic

import crdtver.language.TypedAst.{IdType, InTypeDecl}
import crdtver.symbolic.ExprTranslation.translateType
import crdtver.symbolic.SVal.{exists, forall, forallL}
import crdtver.utils.MapUtils.MapExtensions

import scala.collection.mutable

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

  def idTypeExpr(idType: InTypeDecl): IdType = {
    require(idType.isIdType)
    IdType(idType.name.name)(idType.source)
  }

  /** function that returns the unique identifiers of type t in an operation */
  def uniqueIds_op[T <: SymbolicSort](op: SVal[T], t: IdType)(implicit ctxt: SymbolicContext): SVal[SortSet[SortCustomUninterpreted]] = {
    val returnType = SortSet(translateType(t))
    val func = UninterpretedFunction(s"uniqueIds_op_${t.name}_${op.typ}",
      List(op.typ), returnType)
    SFunctionCall(returnType, func, List(op))
  }

}
