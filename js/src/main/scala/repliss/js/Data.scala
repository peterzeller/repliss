package repliss.js

import repliss.js.Data.ResultState.Valid

object Data {

  case class Example(
    name: String,
    code: String
  ) {
    override def toString: String = name
  }

  case class ReplissResult(
    procedures: List[String] = List(),
    verificationResults: List[VerificationResult] = List(),
    errors: List[ReplissError] = List()
  ) {
    def proceduresWithResult: Map[String, Option[VerificationResult]] =
      (for (p <- procedures) yield
        p -> (for (v <- verificationResults.find(_.procedureName == p)) yield v)).toMap

    def hasErrors: Boolean = {
      verificationResults.exists(r => r.resultState != Valid) || errors.nonEmpty
    }

  }

  /*
                <error line="1" column="0" endline="1" endcolumn="0" message="mismatched input 'Loading' expecting {&lt;EOF&gt;, 'idtype', 'type', 'operation', '@inline', 'query', 'axiom', 'def', 'crdt', 'invariant'}"/>
   */
  case class ReplissError(
    line: Int,
    column: Int,
    endLine: Int,
    endColumn: Int,
    message: String
  )

  case class VerificationResult(
    procedureName: String,
    time: String,
    resultState: ResultState,
    translations: List[Translation],
    trace: List[TraceStep]
  )

  case class TraceStep(
    line: Int,
    description: String,
    info: CounterExample
  )

  case class CounterExample(
    modelText: String,
    svg: String
  )

  case class Translation(
    name: String,
    isabelleTranslation: String,
    smtTranslation: String
  )

  sealed abstract class ResultState
  object ResultState {

    object Valid extends ResultState

    object Error extends ResultState

    object Unknown extends ResultState

  }

}
