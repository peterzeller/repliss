package crdtver.symbolic

import crdtver.language.TypedAst.InProgram
import crdtver.symbolic.SymbolicContext.Model
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, LocalVar, SnapshotTime, TransactionId, TransactionInfo}
import crdtver.testing.{Interpreter, Visualization}
import crdtver.utils.PrettyPrintDoc.Doc
import crdtver.utils.{IdGenerator, MapWithDefault}

import scala.collection.MapView

object ModelExtraction {

  private def debugPrint(s: String): Unit = {
    println(s)
  }

  def extractModel(state: SymbolicState, model: Model, prog: InProgram): SymbolicCounterExampleModel = {

    val iState: Interpreter.State = extractInterpreterState(state, model)

    debugPrint(s"STATE =\n$iState")

    val modelText: Doc =
      s"""${printModel(model, state)}
         |
         |Interpreted state:
         |----------------
         |
         |${printInterpreterState(iState)}
         |""".stripMargin

    val renderResult = Visualization.renderStateGraph(prog, iState)
    SymbolicCounterExampleModel(
      iState,
      modelText,
      renderResult
    )
  }

  def extractInterpreterState(state: SymbolicState, model: Model): Interpreter.State = {

    debugPrint(s"callsS = ${model.evaluate(state.calls)}")

    def normalize(v: SVal[_ <: SymbolicSort]): String = {
      model.evaluate(v).toString
    }

    val translateCallId: IdGenerator[SVal[SortCallId], String, CallId] =
      new IdGenerator(normalize, CallId(_))

    val translateTransactionId: IdGenerator[SVal[SortTxId], String, TransactionId] =
      new IdGenerator(normalize, TransactionId(_))

    val translateInvocationId: IdGenerator[SVal[SortInvocationId], String, InvocationId] =
      new IdGenerator(normalize, InvocationId(_))

    def getSnapshotTime(c: SVal[SortCallId]): SnapshotTime = {
      val hb = model.evaluate(state.happensBefore.get(c))
      SnapshotTime(extractSet(hb).map(translateCallId))
    }

    def getCallTransaction(c: SVal[SortCallId]): TransactionId = {
      val to: SVal[SortOption[SortTxId]] = model.evaluate(state.callOrigin.get(c))
      to match {
        case SNone(ty) =>
          debugPrint(s"!!! Call $c is not in a transaction")
          TransactionId(-1)
        case SSome(t) =>
          translateTransactionId(t)
        case _ => ???
      }
    }

    def getTransactionOrigin(tx: SVal[SortTxId]): InvocationId = {
      val io: SVal[SortOption[SortInvocationId]] = model.evaluate(state.transactionOrigin.get(tx))
      io match {
        case SNone(ty) =>
          debugPrint(s"!!! Transaction $tx which is not in an invocation")
          InvocationId(-2)
        case SSome(i) =>
          translateInvocationId(i)
        case _ => ???
      }
    }

    def getCallInvocation(c: SVal[SortCallId]): InvocationId = {
      val to: SVal[SortOption[SortTxId]] = model.evaluate(state.callOrigin.get(c))
      to match {
        case SNone(ty) =>
          debugPrint(s"!!! Call $c is not in a transaction")
          InvocationId(-1)
        case SSome(tx) =>
          getTransactionOrigin(tx)
        case _ => ???
      }
    }

    def translateCall(value: SVal[SortCall]): DataTypeValue = value match {
      case SCallInfo(o, args) =>
        DataTypeValue(o, args.map(AnyValue(_)))
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }

    def translateInvocationOp(value: SVal[SortInvocationInfo]): DataTypeValue = value match {
      case SInvocationInfo(procName, args) =>
        DataTypeValue(procName, args.map(AnyValue(_)))
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }

    def translateInvocationRes(value: SVal[SortInvocationRes]): Option[DataTypeValue] = value match {
      case SReturnVal(m, v) =>
        Some(DataTypeValue(m, List(AnyValue(v))))
      case SReturnValNone() =>
        None
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }


    val scalls: Map[SVal[SortCallId], SVal[SortCall]] = extractMap(model.evaluate(state.calls))
    val sInvocationOp: Map[SVal[SortInvocationId], SVal[SortInvocationInfo]] = extractMap(model.evaluate(state.invocationOp))
    val sInvocationRes: Map[SVal[SortInvocationId], SVal[SortInvocationRes]] = extractMap(model.evaluate(state.invocationRes))

    // TODO replace this with tree traversal collecting all occurrences
    for (c <- scalls.keys) {
      translateCallId(c)
    }


    for (i <- sInvocationOp.keys) {
      translateInvocationId(i)
    }
    for (i <- sInvocationRes.keys) {
      translateInvocationId(i)
    }
    translateInvocationId(state.currentInvocation)
    for ((ci, i) <- translateInvocationId) {
      val iCalls: Set[SVal[SortCallId]] = extractSet(model.evaluate(state.invocationCalls.get(ci)))
      for (c <- iCalls)
        translateCallId(c)
    }

    // TODO enable safety check
    translateCallId.freeze()
    translateInvocationId.freeze()


    val invocationCalls: Map[InvocationId, Set[CallId]] =
      (for ((si, i) <- translateInvocationId) yield {
        val cs = extractSet(model.evaluate(state.invocationCalls.get(si)))
        i -> cs.map(translateCallId)
      }).toMap


    val calls: Map[CallId, CallInfo] =
      (for ((sc, c) <- translateCallId) yield {
        val tx = getCallTransaction(sc)
        val invoc = getCallInvocation(sc)

        c -> CallInfo(
          c,
          translateCall(scalls(sc)),
          getSnapshotTime(sc),
          tx,
          invoc,
        )
      }).toMap

    debugPrint(s"calls = $calls")

    //    for ((i,cs) <-invocationCalls) {
    //      for (c <- cs) {
    //        calls.get(c) match {
    //          case Some(ci) =>
    //            if (ci.origin != i)
    //              throw new RuntimeException(s"Call $c from ${ci.callTransaction}/${ci.origin} should not appear in $i\n$invocationCalls")
    //          case None =>
    //            throw new RuntimeException(s"Could not find call info for $c in $i")
    //        }
    //      }
    //    }


    val transactions: Map[TransactionId, TransactionInfo] =
      (for ((st, t) <- translateTransactionId) yield {
        val callsInT: List[CallInfo] = calls.values.filter(_.callTransaction == t).toList
        t -> TransactionInfo(
          t,
          SnapshotTime(intersection(callsInT
            .map(_.callClock.snapshot))),
          getTransactionOrigin(st),
          callsInT,
          true
        )
      }).toMap

    debugPrint(s"transactions = $transactions")

    val invocations: Map[InvocationId, InvocationInfo] =
      (for ((si, i) <- translateInvocationId) yield {
        val invocationOp: SVal[SortInvocationInfo] = model.evaluate(state.invocationOp.get(si))
        val invocationRes: SVal[SortInvocationRes] = model.evaluate(state.invocationRes.get(si))
        i -> InvocationInfo(
          i,
          translateInvocationOp(invocationOp),
          translateInvocationRes(invocationRes)
        )
      }).toMap

    debugPrint(s"invocations = $invocations")

    val currentInvoc: InvocationId = translateInvocationId(state.currentInvocation)

    val varValues: Map[LocalVar, AnyValue] =
      for ((v, x) <- state.localState) yield
        LocalVar(v.name) -> AnyValue(model.evaluate(x))


    val localState = Interpreter.LocalState(
      varValues,
      List(), Interpreter.WaitForNothing(), None, Set()
    )

    Interpreter.State(
      calls = calls,
      transactions = transactions,
      invocations = invocations,
      localStates = Map(currentInvoc -> localState)
    )
  }


  private def extractMap[K <: SymbolicSort, V <: SymbolicSort](cs: SVal[SortMap[K, V]]): Map[SVal[K], SVal[V]] = cs match {
    case SymbolicMapUpdated(k, v, b) =>
      extractMap(b) + (k -> v)
    case m@SymbolicMapEmpty(dv) =>
      dv match {
        case SNone(_) =>
          Map()
        case d =>
          new MapWithDefault(Map(), d)
      }
    case x =>
      throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
  }

  private def extractSet[T <: SymbolicSort](s: SVal[SortSet[T]]): Set[SVal[T]] = s match {
    case SSetInsert(base, v) =>
      extractSet(base) ++ v
    case SSetEmpty() =>
      Set()
    case SSetUnion(a, b) =>
      extractSet(a) ++ extractSet(b)
    case x =>
      throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
  }

  private def extractBool(s: SVal[SortBoolean]): Boolean = s match {
    case SBool(value) => value
    case x =>
      throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
  }

  def intersection[T](list: List[Set[T]]): Set[T] = list match {
    case List() => Set()
    case List(s) => s
    case x :: xs =>
      x.intersect(intersection(xs))
  }

  def printModel(model: Model, state: SymbolicState): Doc = {
    import crdtver.utils.PrettyPrintDoc._

    implicit def exprToDoc(e: SVal[_]): Doc = e.toString

    implicit def mapToDoc[K, V](m: Map[K, V]): Doc =
      nested(2, line <> sep(line, m.map((e: (K, V)) => e._1.toString <+> "->" <+> e._2.toString).toList))

    implicit def mapViewToDoc[K, V](m: MapView[K, V]): Doc =
      nested(2, line <> sep(line, m.map((e: (K, V)) => e._1.toString <+> "->" <+> e._2.toString).toList))

    implicit def setToDoc[T](m: Set[T]): Doc =
      "{" <> sep(", ", m.map("" <> _.toString).toList) <> "}"

    state.trace.toString </>
      "calls = " <> extractMap(model.evaluate(state.calls)) </>
      "happensBefore = " <> extractMap(model.evaluate(state.happensBefore)).view.mapValues(extractSet) </>
      "callOrigin = " <> extractMap(model.evaluate(state.callOrigin)) </>
      "transactionOrigin = " <> extractMap(model.evaluate(state.transactionOrigin)) </>
      //      "generatedIds = " <> model.evaluate(state.generatedIds) </>
      //      "knownIds = " <> model.evaluate(state.knownIds) </>
      "invocationCalls = " <> extractMap(model.evaluate(state.invocationCalls)).view.mapValues(extractSet) </>
      "invocationOp = " <> extractMap(model.evaluate(state.invocationOp)) </>
      "invocationRes = " <> extractMap(model.evaluate(state.invocationRes)) </>
      "currentInvocation = " <> model.evaluate(state.currentInvocation) </>
      "currentTransaction = " <> state.currentTransaction.map(v => model.evaluate(v)).toString </>
      "localState = " <> sep(line, state.localState.toList.map { case (k, v) => k.name <+> ":=" <+> model.evaluate(v) }) </>
      "visibleCalls = " <> extractSet(model.evaluate(state.visibleCalls)) </>
      "currentCallIds = " <> sep(", ", state.currentCallIds.map("" <> model.evaluate(_))) </>
      "satisfiable = " <> state.satisfiable.toString
  }

  private def printInterpreterState(state: Interpreter.State): Doc = {
    import crdtver.utils.PrettyPrintDoc._

    //    calls: Map[CallId, CallInfo] = Map(),
    //        //    transactionCalls: Map[TransactionId, Set[CallId]] = Map(),
    //        maxCallId: Int = 0,
    //        transactions: Map[TransactionId, TransactionInfo] = Map(),
    //        maxTransactionId: Int = 0,
    //        invocations: Map[InvocationId, InvocationInfo] = Map(),
    //        maxInvocationId: Int = 0,
    //        // returned Ids for each id-type and the invocation that returned it
    //        knownIds: Map[IdType, Map[AnyValue, InvocationId]] = Map(),
    //        localStates: Map[InvocationId, LocalState] = Map()

    "calls: " <> nested(2, line <> sep(line, state.calls.values.map(c =>
      c.id.toString <+> "->" <+> c.operation.toString <>
        nested(2, line <> "clock = " <> c.callClock.toString </>
          "tx = " <> c.callTransaction.toString </>
          "invoc = " <> c.origin.toString)
    ))) </>
      "invocations: " <> nested(2, line <> sep(line, state.invocations.values.map(i =>
      i.id.toString <+> "->" <+> i.operation.toString <+> ", returned " <+> i.result.toString))) </>
      "transactions: " <> nested(2, line <> sep(line, state.transactions.values.map(tx =>
      tx.id.toString <+> "->" <+> tx.currentCalls.toString <+> " from " <+> tx.origin.toString)))
  }

}
