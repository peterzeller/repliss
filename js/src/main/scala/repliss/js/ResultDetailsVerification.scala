package repliss.js

import repliss.js.Data.{TraceStep, Translation, VerificationError, VerificationResult}
import repliss.js.Utils.unescape
import slinky.core.ComponentWrapper
import slinky.core.facade.ReactElement
import slinky.web.html.{step, _}

import scala.scalajs.js

/*
case class VerificationResult(
    procedureName: String,
    time: String,
    resultState: ResultState,
    translations: List[Translation],
    trace: List[TraceStep]
  )

 */

object ResultDetailsVerification extends ComponentWrapper {
  type Props = VerificationResult

  case class State()

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = State()


    def renderTranslations(): ReactElement = {
      val ts = props.translations ++ props.verificationError.map(_.translation)

      TranslationViewer(Utils.uniqueKeys(ts)(_.name)((e,v) => e.copy(name = v)))
    }

    override def render(): ReactElement = {
      div()(
        h2(s"Verification Results for ${props.procedureName}:"),
        p(s"Time: ${props.time}"),
        props.verificationError match {
          case Some(e) =>
            div(
              p(className := "errorMessage")(e.message),
              TraceViewer(e)
            )
          case None => ""
        }
        ,
        renderTranslations()
      )
    }
  }

}


object TraceViewer extends ComponentWrapper {
  type Props = VerificationError

  case class State(selected: String = "")

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = State()


    override def render(): ReactElement = {
      val selectedStepOpt: Option[TraceStep] =
        props.trace.find(_.key == state.selected).orElse(props.trace.lastOption)

      selectedStepOpt match {
        case None =>
          ""
        case Some(step) =>
          div(className := "traceViewer")(
            h3("Error Trace: "),
            div(className := "stepSelection")(
              ul(
                for (s <- props.trace) yield
                  li(
                    key := s.key,
                    className := (if (selectedStepOpt.contains(s)) "current" else ""),
                    onClick := (() => setState(state.copy(selected = s.key)))
                  )(s"line ${s.line}: ${s.description}")
              )
            ),
            div(className := "selectionDetails")(
              div(className := "description")(
                step.description
              ),
              div(className := "visualization")(
                SvgViewer(SvgViewer.Props(unescape(step.info.svg).trim))
              )
            ),
            div(className := "clear")
          )
      }


    }
  }
}




object TranslationViewer extends ComponentWrapper {
  type Props = List[Translation]

  case class State(selected: String = "", isabelle: Boolean = true) {
    def toggle(key: String): State =
      copy(selected = (if (key == selected) ""else key))

  }

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = State()


    override def render(): ReactElement = {
      val selectedTranslation: Option[Translation] =
        props.find(_.key == state.selected)

      div(className := "traceViewer")(
        h3("Verification Conditions: "),
        div(className := "stepSelection")(
          ul(
            for (s <- props) yield
              li(
                key := s.key,
                className := (if (selectedTranslation.contains(s)) "current" else ""),
                onClick := (() => setState(state.toggle(s.key)))
              )(s.name)
          )
        ),
        div(className := "selectionDetails")(
          div(className := "description")(
            "Select Language:",
            select(
              onChange := (e => setState(state.copy(isabelle =  e.target.value == "isabelle"))),
              value := (if (state.isabelle) "isabelle" else "cvc4")
            )(
              option(value := "isabelle")("Isabelle"),
              option(value := "cvc4")("Cvc4")

            )
          ),
          div(className := "visualization")(
            selectedTranslation match {
              case None => ""
              case Some(tr) =>
                AceEditor(AceEditor.Props(code = if (state.isabelle) tr.isabelleTranslation else tr.smtTranslation, 14, new Portal[String]))
            }
          )
        ),
        div(className := "clear")
      )
    }
  }
}
