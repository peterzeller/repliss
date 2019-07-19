package repliss.js

import repliss.js.Data.ReplissResult
import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

@react class App extends StatelessComponent {
  type Props = Unit

  private val css = AppCSS

  def render() = {
    div(className := "App")(
      header(className := "App-header")(
        h1(className := "App-title")("Welcome to React (with Scala.js!)")
      ),
      ResultView(ReplissResult()),
      AceEditor("Code")
    )
  }
}
