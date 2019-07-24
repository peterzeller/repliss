package repliss.js

import repliss.js.Data.QuickCheckCounterExample
import repliss.js.Utils.unescape
import slinky.core.ComponentWrapper
import slinky.core.facade.ReactElement
import slinky.web.html._
import slinky.web.svg.svg

import scala.scalajs.js


object ResultDetailsQuickCheck extends ComponentWrapper {
  type Props = QuickCheckCounterExample
  case class State()

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = State()



    override def render(): ReactElement = {
      val unescaped = unescape(props.counterExampleSvg).trim
      div()(
        h2("QuickCheck counterexample:"),
        p(s"Invariant in line ${props.invLine} failed."),
        div()(
          for (i <- props.info.split(";").toList) yield p(i)),
        SvgViewer(SvgViewer.Props(unescaped))
      )
    }
  }


}
