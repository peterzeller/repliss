package repliss.js

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSImport, ScalaJSDefined}
import Data._
import slinky.core.facade.ReactElement

@js.native @JSGlobal
object ace extends js.Object {
  def edit(id: String): AceApi = js.native
}

trait AceApi extends js.Object {

  def setTheme(theme: String): Unit
  def getSession(): AceSession
  def setShowPrintMargin(b: Boolean): Unit
  def setAutoScrollEditorIntoView(): Unit
  def setOptions(options: js.Object): Unit
}
trait AceSession extends js.Object {
  def setMode(m: String): Unit
}


@react class AceEditor extends StatelessComponent {
  type Props = String

  override def componentDidMount(): Unit = {
    Console.println("AceEditor componentDidMount")
    super.componentDidMount()
    val editor = ace.edit("ace-editor")
    editor.setTheme("ace/theme/github")
    editor.getSession().setMode("ace/mode/repliss")
    editor.setShowPrintMargin(false)
    editor.setAutoScrollEditorIntoView()
    editor.setOptions(js.Dynamic.literal(
        maxLines = Double.PositiveInfinity
    ))

  }


  def render(): ReactElement = {
    Console.println("AceEditor Render")
    div(
      script(src := "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.5/ace.js"),
      div(id := "ace-editor", "Loading editor")
    )
  }
}

