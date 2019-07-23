package repliss.js

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import Data._
import slinky.core.facade.ReactElement

sealed trait DetailData
case class VerificationDetails(procName: String) extends DetailData
object QuickCheckDetails extends DetailData

object ResultView extends ComponentWrapper {
  type Props = Option[ReplissResult]
  case class State(
    details: Option[DetailData] = None
  )


  private val css = AppCSS

  class Def(jsProps: js.Object) extends Definition(jsProps) {

    override def initialState: State = State()

    def calculateClassName(result: ReplissResult): String = {
      if (result.hasErrors) {
        "bg-danger"
      } else {
        "bg-success"
      }
    }

    def render(): ReactElement = {
      props match {
        case None =>
          div()
        case Some(result) =>

          div(id:= "output", className := calculateClassName(result))(
            ul(
              if (result.errors.isEmpty) {
                (for ((proc, r) <- result.proceduresWithResult) yield
                  renderProcVerificationResult(proc, r)) ++
                  List(renderQuickCheckResult(result.quickcheckResult))
              } else {
                for ((err, i) <- result.errors.zipWithIndex) yield
                  li(key := s"error-$i")(s"Error in line ${err.line}: ${err.message}")
              }
            ),
            div()(
              state.details match {
                case Some(details) =>
                  details match {
                    case VerificationDetails(procName) =>
                      val resOpt: Option[ReactElement] = props.flatMap(res => res.verificationResults.find(_.procedureName == procName))
                        .map((vr: VerificationResult) => ResultDetailsVerification(vr))
                      resOpt.getOrElse[ReactElement]("")
                    case QuickCheckDetails =>
                      props.flatMap(res => res.quickcheckResult).flatMap(res => res match {
                        case Data.QuickCheckResultOk =>None
                        case q: QuickCheckCounterExample => Some(q)
                      }) match {
                        case Some(r) =>
                          ResultDetailsQuickCheck(r)
                        case None =>
                          ""
                      }
                  }
                case None => ""
              }
            )
          )
      }

    }

    private def renderQuickCheckResult(qr: Option[QuickCheckResult]) = {
      var s: ReactElement = span(className := "spinner")
      var details: ReactElement = span("")

      qr match {
        case None =>
        case Some(res) =>
          res match {
            case Data.QuickCheckResultOk =>
              s = "✓"
            case q: QuickCheckCounterExample =>
              s = "✗"
              details = detailsToggle(QuickCheckDetails)
          }
      }


      li(key := "_quickcheck")(
        span(
          span(className := "result-status")(s),
          "QuickCheck Random tests",
          span(className := "result-details")(details)
        )
      )
    }

    private def detailsToggle(details: DetailData): ReactElement = {
      var newDetails: Option[DetailData] = Some(details)
      var text: String = "(show details)"
      if (state.details.contains(details)) {
        newDetails = None
        text  = "(hide details)"
      }

      a(onClick := (() => {
        setState(state.copy(details = newDetails))
      }))(text)
    }

    private def renderProcVerificationResult(proc: String, r: Option[VerificationResult]) = {
      var s: ReactElement = span(className := "spinner")
      var details: ReactElement = span("")

      r match {
        case None =>
        case Some(res) =>
          details = detailsToggle(VerificationDetails(res.procedureName))
          res.resultState match {
            case ResultState.Valid =>
              s = "✓"
            case ResultState.Error =>
              s = "✗"
            case ResultState.Unknown =>
              s = "⁇"
          }
      }

      li(key := proc)(
        span(
          span(className := "result-status")(s),
          proc,
          span(className := "result-details")(details)
        )
      )
    }
  }
}


