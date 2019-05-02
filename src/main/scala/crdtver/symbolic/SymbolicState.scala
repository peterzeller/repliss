package crdtver.symbolic

import crdtver.language.TypedAst.{AstElem, IdType, SourceTrace}

/**
  * The state of the system.
  */
case class SymbolicState(
  calls: SymbolicMap[SortCallId, SortCall],
  // call -> set of calls that happened before
  happensBefore: SymbolicMap[SortCallId, SortSet[SortCallId]],
  callOrigin: SymbolicMap[SortCallId, SortOption[SortTxId]],
  transactionOrigin: SymbolicMap[SortTxId, SortOption[SortInvocationId]],
  // TODO can remove transactionStatus, since all transactions are committed at interesting points
//  transactionStatus: SymbolicMap[SortTxId, SortOption[SortTransactionStatus]],
  generatedIds: Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]],
  knownIds: Map[IdType, SVal[SortSet[SortCustomUninterpreted]]],
  invocationCalls: SymbolicMap[SortInvocationId, SortSet[SortCallId]],
  invocationOp: SymbolicMap[SortInvocationId, SortInvocationInfo],
  invocationRes: SymbolicMap[SortInvocationId, SortInvocationRes],
  currentInvocation: SVal[SortInvocationId],
  currentTransaction: Option[SVal[SortTxId]] = None,
  localState: Map[ProgramVariable, SVal[_ <: SymbolicSort]],
  visibleCalls: SymbolicSet[SortCallId],
  // for optimizations:
  // store calls that have been made in the current invocation so that we can easily add distinct constraint
  currentCallIds: List[SVal[SortCallId]] = List(),
  satisfiable: Boolean = true,
  // trace including the state after each step
  trace: Trace[SymbolicState],
  // constraints that need to hold:
  constraints: List[NamedConstraint] = List()
) {


  def lookupLocal(name: String): SVal[_ <: SymbolicSort] =
    localState.get(ProgramVariable(name)) match {
      case Some(value) =>
        value
      case None =>
        throw new RuntimeException(s"could not find variable $name in state $localState")
    }

  def withLocal(pv: ProgramVariable, v: SVal[_ <: SymbolicSort]): SymbolicState =
    this.copy(
      localState = localState + (pv -> v)
    )

  def withTrace(description: String, source: SourceTrace): SymbolicState = {
    this.copy(
      trace = this.trace + TraceStep(description, this, source)
    )
  }

  def withTrace(description: String, source: AstElem): SymbolicState = {
    withTrace(description, source.getSource())
  }


  def allConstraints(): List[NamedConstraint] =
    constraints.reverse

  def withConstraint(what: String, constraint: SVal[SortBoolean]): SymbolicState = {
    this.copy(
      constraints = NamedConstraint(what, constraint) :: constraints
    )
  }

  def withConstraints(newConstraints: Iterable[NamedConstraint]): SymbolicState =
    this.copy(constraints = newConstraints.toList ++ constraints)
}

case class ProgramVariable(name: String)

case class Trace[Info](
  // steps in reverse order
  private val stepsR: List[TraceStep[Info]] = List()
) {
  def steps: List[TraceStep[Info]] = stepsR.reverse

  def lastStep: Option[TraceStep[Info]] = stepsR.headOption

  /** transforms the information stored in the trace */
  def mapInfo[T](f: Info => T): Trace[T] =
    Trace(stepsR.map(i => TraceStep(i.description, f(i.info), i.source)))

  def +(step: TraceStep[Info]): Trace[Info] =
    Trace(step :: stepsR)

  override def toString: String = {
    val r = new StringBuilder("Trace:\n")
    for (s <- stepsR.reverse) {
      r.append(s"line ${s.source.getLine}: ")
      r.append(s.description)
      r.append("\n")
    }
    r.toString()
  }

}


case class TraceStep[Info](
  description: String,
  // some additional information stored with the trace
  info: Info,
  source: SourceTrace
)
