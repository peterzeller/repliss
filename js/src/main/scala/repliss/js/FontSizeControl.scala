package repliss.js

import repliss.js.FontSizeControl.P
import slinky.core.Component
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._


@react class FontSizeControl extends Component {
  type State = Int

  override type Props = P


  override def initialState: Int = 14

  override def render(): ReactElement = {
    div(className := "fontsize")(
      "Font size:",
      button(id := "decreaseEditorFont", className := "btn", onClick := (() => change(-1)))("-"),
      button(id := "increaseEditorFont", className := "btn", onClick := (() => change(1)))("+")
    )
  }

  def change(i: Int): Unit = {
    val newState = state + i
    setState(newState)
    props.onChange(newState)
  }
}

object FontSizeControl {
  case class P(onChange: Int => Unit)
}