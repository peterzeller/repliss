package repliss.js

object Data {

  case class Example(
    name: String,
    code: String
  ) {
    override def toString: String = name
  }

  case class ReplissResult(
    procedures: List[String] = List(),
    verificationResults: List[VerificationResult] = List()
  )

  case class VerificationResult(
    procedureName: String,
    time: Double,
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
