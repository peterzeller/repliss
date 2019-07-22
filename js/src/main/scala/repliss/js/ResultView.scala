package repliss.js

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import Data._
import slinky.core.facade.ReactElement

object ResultView extends ComponentWrapper {
  type Props = Option[ReplissResult]
  case class State()

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
            )
          )
      }

    }

    private def renderQuickCheckResult(qr: Option[QuickCheckResult]) = {
      li(key := "_quickcheck")(
        span(
          span(className := "result-status")(
            qr match {
              case None =>span(className := "spinner")
              case Some(res) =>
                res match {
                  case Data.QuickCheckResultOk => "✓"
                  case q: QuickCheckCounterExample => "✗"
                }
            }
          ),
          "QuickCheck Random tests"
        )
      )
    }

    private def renderProcVerificationResult(proc: String, r: Option[VerificationResult]) = {
      li(key := proc)(
        span(
          span(className := "result-status")(
            r match {
            case None =>span(className := "spinner")
            case Some(res) =>
              res.resultState match {
                case ResultState.Valid => "✓"
                case ResultState.Error => "✗"
                case ResultState.Unknown => "⁇"
              }
          }
          ),
          proc
        )
      )
    }
  }
}


