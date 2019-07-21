package repliss.js

import repliss.js.App.S
import repliss.js.Data.ReplissResult
import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

@react class App extends Component {
  type State = S
  type Props = Unit

  private val css = AppCSS

  override def initialState: S = S()

  def render() = {
    div(className := "container-fluid")(
      div(className := "page-header")(
        h1()(
          "Repliss",
          small()("Verification Tool for Replicated Information Systems")
        )
      ),
      FontSizeControl(FontSizeControl.P(onChange = s => this.setState(state.copy(editorFontSize =  s)))),
      nav(className := "navbar navbar-default")(
        div(className := "container-fluid")(
          ul(className := "nav navbar-nav")(
            li()(
              button(id := "btn-verify", `type` := "button", className := "btn btn-default navbar-btn btn-success")(
                span(className := "spinner"),
                "Check with Repliss"
              )
            ),
            li()(
              p(className := "navbar-text")("Choose example:")
            ),
            Dropdown(Dropdown.P(
              state.examples,
              state.selectedExample,
              (s: String) => setState(state.copy(selectedExample = s))))
          )
        )
      ),
      ResultView(ReplissResult()),
      AceEditor(AceEditor.P(
        code = "Code",
        fontSize = state.editorFontSize
      ))
    )
  }
}

object App {
  case class S(
    editorFontSize: Int = 14,
    selectedExample: String = "A",
    examples: List[String] = List("A", "B", "C")
  )
}