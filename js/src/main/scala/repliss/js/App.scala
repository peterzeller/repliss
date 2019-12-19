package repliss.js

import repliss.js.Data.ReplissResult
import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom.window
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
    codePortal: Portal[String] = new Portal[String](),
    examples: List[Data.Example] = List(),
    replissResult: Option[Data.ReplissResult] = None
  )

  implicit val ctx: Ctx.Owner = Ctx.Owner.safe()


  class Def(jsProps: js.Object) extends Definition(jsProps) {
    def onVerifyClick(): Future[_] = {
      setState(state.copy(replissResult = None))

      val p: Promise[Unit] = Promise[Unit]

      val res: Var[ReplissResult] = ReplissApi.check(state.codePortal.read)

      res.triggerLater(r => {
        p.trySuccess(())
        this.setState(state.copy(replissResult = Some(r)))
      })

      p.future
    }


    private val css = AppCSS

    override def initialState: State = State()

    override def componentDidMount(): Unit = {
      ReplissApi.getExamples.onComplete {
        case Failure(exception) =>
          Console.println("Failed to load examples")
          setState(state.copy(
            selectedExample = Data.Example("Failed to load examples", "Failed to load examples ...\nPlease try to refresh the page")
          ))
        case Success(examples) =>
          val key = window.location.hash.replaceAll("^\\#", "")
          setState(state.copy(
            selectedExample = examples.find(_.key == key).getOrElse(examples.head),
            examples = examples
          ))
      }
    }

    def render(): ReactElement = {
      div()(
        VersionInfo(),
        nav(className := "navbar navbar-default")(
          div(className := "container-fluid")(
            ul(className := "nav navbar-nav")(
              li()(
                VerifyButton(VerifyButton.Props(onClick = _ => this.onVerifyClick() ))
              ),
              li()(
                p(className := "navbar-text")("Choose example:")
              ),
              Dropdown(Dropdown.Props(
                state.examples,
                state.selectedExample,
                onChange = (s: Data.Example) => {
                  setState(state.copy(selectedExample = s))
                })),
            ),
            ul(className := "nav navbar-nav navbar-right")(
              li()(
                FontSizeControl(FontSizeControl.Props(onChange = s => this.setState(state.copy(editorFontSize =  s))))
              )
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