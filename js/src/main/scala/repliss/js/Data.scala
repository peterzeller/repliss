package repliss.js

import repliss.js.Data.ResultState.Valid

object Data {

  case class Example(
    name: String,
    code: String
  ) {
    def key: String = name.replaceAll("[^a-zA-Z0-9]+", "-")

    override def toString: String = name
  }

  sealed trait ResultStatus
  object ResultStatusOk extends ResultStatus
  case class ResultStatusErr(code: Int) extends ResultStatus
  object ResultStatusRunnning extends ResultStatus

  case class ReplissResult(
    procedures: List[String] = List(),
    verificationResults: List[VerificationResult] = List(),
    errors: List[ReplissError] = List(),
    quickcheckResult: Option[QuickCheckResult] = None,
    resultStatus: ResultStatus = ResultStatusRunnning
  ) {
    def proceduresWithResult: Map[String, Option[VerificationResult]] =
      (for (p <- procedures) yield
        p -> (for (v <- verificationResults.find(_.procedureName == p)) yield v)).toMap

    def hasErrors: Boolean = {
      resultStatus.isInstanceOf[ResultStatusErr] ||
        verificationResults.exists(r => r.resultState != Valid) ||
        errors.nonEmpty ||
        quickcheckResult.exists(r => r.isInstanceOf[QuickCheckCounterExample])
    }

  }

  sealed trait QuickCheckResult

  object QuickCheckResultOk extends QuickCheckResult

  case class QuickCheckCounterExample(
    invLine: Int,
    info: String,
    render: RenderResult
  )
    extends QuickCheckResult

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
    verificationError: Option[VerificationError]
  )

  case class VerificationError(
    message: String,
    trace: List[TraceStep],
    translation: Translation
  )

  case class TraceStep(
    line: Int,
    description: String,
    info: CounterExample
  ) {
    def key: String = line + "_" + description

  }

  case class CounterExample(
    modelText: String,
    render: RenderResult
  )

  case class RenderResult(
    dot: String,
    svg: String,
    pdf: String
  )

  case class Translation(
    name: String,
    isabelleTranslation: String,
    smtTranslation: String
  ) {
    def key = name

  }

  sealed abstract class ResultState
  object ResultState {

    object Valid extends ResultState

    object Error extends ResultState

    object Unknown extends ResultState

  }

}
