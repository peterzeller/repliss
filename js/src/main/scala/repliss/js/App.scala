package repliss.js

import repliss.js.Data.ReplissResult
import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import monix.execution.Scheduler.Implicits.global

import scala.util.{Failure, Success}

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

object App extends ComponentWrapper {
  type Props = Unit
  case class State(
    editorFontSize: Int = 14,
    selectedExample: Data.Example = Data.Example("Loading examples", "Loading example ..."),
    examples: List[Data.Example] = List()
  )


  class Def(jsProps: js.Object) extends Definition(jsProps) {

    private val css = AppCSS

    override def initialState: State = State()

    override def componentDidMount(): Unit = {
      ReplissApi.getExamples.onComplete {
        case Failure(exception) =>
          exception.printStackTrace()
          Console.println(s"getExamples error $exception")
        case Success(value) =>
          Console.println(s"Got examples $value")
          value match {
            case Left(err) =>
              Console.println(s"Got examples error $err")
              setState(state.copy(selectedExample = Data.Example("Error loading examples",
                s"Error: Examples could not be loaded: ${err.getMessage}")))
            case Right(examples) =>
              Console.println(s"Got examples error $examples")
              setState(state.copy(
                selectedExample = examples.head,
                examples = examples
              ))
          }
      }
    }

    def render() = {
      div(className := "container-fluid")(
        div(className := "page-header")(
          h1()(
            "Repliss",
            small()("Verification Tool for Replicated Information Systems")
          )
        ),
        FontSizeControl(FontSizeControl.Props(onChange = s => this.setState(state.copy(editorFontSize =  s)))),
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
              Dropdown(Dropdown.Props(
                state.examples,
                state.selectedExample,
                (s: Data.Example) => setState(state.copy(selectedExample = s))))
            )
          )
        ),
        ResultView(ReplissResult()),
        AceEditor(AceEditor.Props(
          code = state.selectedExample.code,
          fontSize = state.editorFontSize
        ))
      )
    }
  }
}