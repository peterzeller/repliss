package repliss.js

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import Data._
import slinky.core.facade.ReactElement

object ResultView extends ComponentWrapper {
  type Props = ReplissResult
  case class State()

  private val css = AppCSS

  class Def(jsProps: js.Object) extends Definition(jsProps) {

    override def initialState: State = State()

    def render(): ReactElement = {
      div(
        p(props.toString)
      )
    }
  }
}


