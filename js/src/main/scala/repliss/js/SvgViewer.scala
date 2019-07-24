package repliss.js

import org.scalajs.dom
import org.scalajs.dom.Blob
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{BlobPropertyBag, Element, HTMLAnchorElement, URL}
import slinky.core.ComponentWrapper
import slinky.core.facade.{React, ReactElement, ReactRef}
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.Dynamic
import scala.scalajs.js.annotation.JSImport


@JSImport("svg2pdf.js/dist/svg2pdf.min", JSImport.Namespace)
@js.native
object svg2pdf extends js.Object {
  def apply(svgCode: Element, jspdf: jsPdfElement, d: js.Dynamic): Unit = js.native
}

@JSImport("jspdf-yworks/dist/jspdf.min", JSImport.Namespace)
@js.native
object jsPDF extends js.Object {
  def apply(l: String, r: String, a: js.Array[_]): jsPdfElement = js.native
}

@js.native
trait jsPdfElement extends js.Any {
  def output(mode: String): String

}

/**
 * An svg viewer with download functionality
 */
object SvgViewer extends ComponentWrapper {
  case class Props(svgCode: String)
  type State = Unit

  class Def(jsProps: js.Object) extends Definition(jsProps) {
    override def initialState: State = ()

    val svgRef: ReactRef[Div] = React.createRef

    override def render(): ReactElement = {
      div(className := "svgViewer", ref := svgRef)(
        div(dangerouslySetInnerHTML := js.Dynamic.literal(__html = props.svgCode)),
        a(onClick := (() => downloadSvg()))("Download SVG"),
        a(onClick := (() => downloadPdf()))("Download PDF")
      )
    }

    private def downloadSvg(): Unit = {
      triggerDownload("repliss.svg",  textDownload(props.svgCode))
    }

    private def downloadPdf(): Unit = {
      val svg = svgRef.current.getElementsByTagName("svg")(0)
      val w = svg.getAttribute("width").replaceAll("[^0-9]", "")
      val h = svg.getAttribute("height").replaceAll("[^0-9]", "")
      val pdf = jsPDF("l", "px", js.Array(w, h))
      svg2pdf(svg, pdf, Dynamic.literal(
          xOffset= 0,
          yOffset= 0,
          scale= 1
      ))

      val uri = pdf.output("datauristring")
      triggerDownload("repliss.pdf", uri)
    }

    private def textDownload(content: String): String = {
      val blob = new Blob(js.Array(content),
        BlobPropertyBag(`type` = "text/plain"))
      URL.createObjectURL(blob)
    }

    private def triggerDownload(name: String, url: String): Unit = {
      val dlink: HTMLAnchorElement = dom.document.createElement("a").asInstanceOf[HTMLAnchorElement]
      dlink.setAttribute("download", name)
      dlink.href = url
      dlink.onclick = e => {
        js.timers.setTimeout(1500) {
          URL.revokeObjectURL(dlink.href)
        }
      }

      dom.document.body.appendChild(dlink)
      dlink.click()
      dom.document.body.removeChild(dlink)
//      dlink.remove()

    }

  }
}
