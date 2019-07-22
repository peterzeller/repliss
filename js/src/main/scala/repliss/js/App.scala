package repliss.js

import repliss.js.Data.ReplissResult
import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import monix.execution.Scheduler.Implicits.global
import rx.{Ctx, Obs, Rx, Var}
import slinky.core.facade.ReactElement

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

object App extends ComponentWrapper {
  type Props = Unit
  case class State(
    editorFontSize: Int = 14,
    selectedExample: Data.Example = Data.Example("Loading examples", "Loading example ..."),
    code: String = "Loading example ...",
    codePortal: Portal[String] = new Portal(),
    examples: List[Data.Example] = List(),
    replissResult: Option[Data.ReplissResult] = None
  )

  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()


  class Def(jsProps: js.Object) extends Definition(jsProps) {
    def onVerifyClick(): Future[_] = {
      val p: Promise[Unit] = Promise[Unit]

      val res: Var[ReplissResult] = ReplissApi.check(state.codePortal.read)

      res.triggerLater(r => {
        p.success(())
        this.setState(state.copy(replissResult = Some(r)))
      })

      p.future
    }


    private val css = AppCSS

    override def initialState: State = State()

    override def componentDidMount(): Unit = {
      ReplissApi.getExamples.onComplete {
        case Failure(exception) =>
          exception.printStackTrace()
        case Success(value) =>
          value match {
            case Left(err) =>
              setState(state.copy(selectedExample = Data.Example("Error loading examples",
                s"Error: Examples could not be loaded: ${err.getMessage}")))
            case Right(examples) =>
              setState(state.copy(
                selectedExample = examples.head,
                examples = examples
              ))
          }
      }
    }

    def render(): ReactElement = {
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
                VerifyButton(VerifyButton.Props(onClick = (_ => this.onVerifyClick()) ))
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
        ResultView(state.replissResult),
        AceEditor(AceEditor.Props(
          code = state.selectedExample.code,
          fontSize = state.editorFontSize,
          codePortal = state.codePortal
        ))
      )
    }
  }
}