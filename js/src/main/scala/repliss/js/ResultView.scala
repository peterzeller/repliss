package repliss.js

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import Data._
import slinky.core.facade.ReactElement

@react class ResultView extends Component {
  type Props = ReplissResult
  type State = MyState

  private val css = AppCSS

  override def initialState: State = MyState()

  def render(): ReactElement = {
    div(
      p(props.toString)
    )
  }
}

case class MyState()
