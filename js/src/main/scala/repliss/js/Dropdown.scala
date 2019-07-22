package repliss.js

import slinky.core.{Component, ComponentWrapper}
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._

import scala.scalajs.js

object Dropdown extends ComponentWrapper {
  type T = Data.Example

  case class State(open: Boolean = false)
  case class Props(items: List[T], currentSelection: T, onChange: T => Unit)

  class Def(jsProps: js.Object) extends Definition(jsProps) {

    override def initialState: State = State()

    override def render(): ReactElement = {
      var liClasses = "dropdown"
      if (state.open) {
        liClasses += " open"
      }
      li(className := liClasses)(
        a(href := "#",
          className := "dropdown-toggle",
          data - "toogle" := "dropdown",
          aria - "haspopup" := "true",
          onClick := (() => setState(state.copy(open = !state.open)))
        )( // TODO role="button"
          span(id := "example-dropdown-selection")(props.currentSelection.toString()),
          span(className := "caret")
        ),
        ul(id := "example-dropdown", className := "dropdown-menu")(
          for (item <- props.items) yield
            li(key := item.toString())(a(href := s"#${item.key}",
              onClick := (() => {
                props.onChange(item)
                setState(state.copy(open = false))
              }))(item.toString))
        )
      )
    }
  }

}

