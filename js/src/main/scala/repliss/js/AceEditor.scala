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
  def setValue(value: String, cursorPos: Int): Unit
}
trait AceSession extends js.Object {
  def getValue(): String

  def setMode(m: String): Unit
}


object AceEditor extends StatelessComponentWrapper {

  private var lastPropsCode: String = ""

  case class Props(
      code: String,
      fontSize: Int,
    codePortal: Portal[String]
    )


  class Def(jsProps: js.Object) extends Definition(jsProps) {
    var editor: Option[AceApi] = None

    override def componentDidMount(): Unit = {
      super.componentDidMount()
      val editor = ace.edit("ace-editor")
      this.editor = Some(editor)
      editor.setTheme("ace/theme/github")
      editor.getSession().setMode("ace/mode/repliss")

      editor.setShowPrintMargin(false)
      editor.setAutoScrollEditorIntoView()
      editor.setOptions(js.Dynamic.literal(
        maxLines = Double.PositiveInfinity,
        fontSize = props.fontSize
      ))
      editor.setValue(this.props.code, -1)
      lastPropsCode = this.props.code

      props.codePortal.setHandler(() => {
        editor.getSession().getValue()
      })
    }


    def render(): ReactElement = {
      for (e <- editor) {
        e.setOptions(js.Dynamic.literal(
          maxLines = Double.PositiveInfinity,
          fontSize = props.fontSize
        ))
        if (this.props.code != lastPropsCode) {
          e.setValue(this.props.code, -1)
          lastPropsCode = this.props.code
        }
      }

      div(
        script(src := "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.5/ace.js"),
        div(id := "ace-editor", "Loading editor")
      )
    }
  }
}
