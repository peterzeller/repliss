package repliss.js

import repliss.js.Dropdown.S
import slinky.core.Component
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._


object Dropdown {
  case class S(open: Boolean = false)
  case class P[T](items: List[T], currentSelection: T, onChange: T => Unit)
}



@react class Dropdown extends Component {
  type T = String
  type State = S

  override type Props = Dropdown.P[String]


  override def initialState: State = S()

  override def render(): ReactElement = {
    var liClasses = "dropdown"
    if (state.open) {
      liClasses += " open"
    }
    li(className := liClasses)(
      a(href := "#",
        className := "dropdown-toggle",
        data-"toogle" := "dropdown",
        aria-"haspopup" := "true",
        aria-"expand" := "false",
        onClick := (() => setState(state.copy(open = !state.open)))
      )( // TODO role="button"
        span(id := "example-dropdown-selection")(props.currentSelection.toString()),
        span(className:="caret")
      ),
      ul(id := "example-dropdown", className := "dropdown-menu")(
        for (item <- props.items) yield
          li()(a(href := "#",
            onClick := (() => {
              props.onChange(item)
              setState(state.copy(open = false))
            }))(item.toString))
      )
    )
  }

}

