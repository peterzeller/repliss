package repliss.js

import slinky.core.ComponentWrapper
import slinky.core.facade.ReactElement
import slinky.web.html._

import scala.concurrent.Future
import scala.scalajs.js

import scala.concurrent.ExecutionContext.Implicits.global


object VerifyButton extends ComponentWrapper {
  case class Props(onClick: Unit => Future[_])
  case class State(spinning: Boolean = false)

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = State()

    override def render(): ReactElement = {
      var cssClasses = "btn btn-default navbar-btn btn-success"
      if (state.spinning)
        cssClasses += " running"
      button(id := "btn-verify",
        `type` := "button",
        className := cssClasses,
        onClick := (() => this.onClickHandler()))(
        span(className := "spinner"),
        "Check with Repliss!"
      )
    }

    def onClickHandler(): Unit = {
      setState(state.copy(spinning = true))
      props.onClick(()).onComplete(_ => {
        setState(state.copy(spinning = false))
      })
    }

  }
}
