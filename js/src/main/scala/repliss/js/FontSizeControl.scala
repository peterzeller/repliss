package repliss.js

import slinky.core.{Component, ComponentWrapper}
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._

import scala.scalajs.js


object FontSizeControl extends ComponentWrapper {
  case class Props(onChange: Int => Unit)
  type State = Int

  class Def(jsProps: js.Object) extends Definition(jsProps) {
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
}
