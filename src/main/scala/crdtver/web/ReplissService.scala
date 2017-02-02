package crdtver.web

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.{Calendar, Random}

import com.typesafe.scalalogging.Logger
import crdtver.Repliss
import crdtver.Repliss._
import org.http4s.Request
import org.http4s.dsl.Ok
import org.http4s._
import org.http4s.dsl._
import org.http4s.headers.`Content-Type`

import scalaz.concurrent.Task
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze._
import org.http4s.json4s.native._
import org.json4s.JsonAST._
import org.json4s.{JValue, JsonFormat}
import org.json4s.native.JsonMethods._

import scalatags.Text.TypedTag
import org.json4s.JsonDSL._

case class CheckRequest(code: String)

class ReplissService {

  implicit object CheckRequestFormat extends JsonFormat[CheckRequest] {
    override def read(value: JValue): CheckRequest = CheckRequest(
      code = (value \\ "code" \\ classOf[JString]).head
    )

    override def write(obj: CheckRequest): JValue = Map(
      "code" -> obj.code
    )
  }

  def check(request: Request): Task[Response] = {
    request.as(jsonOf[CheckRequest]).flatMap(checkReq => {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HHmmss-SSS")
      val time = format.format(Calendar.getInstance().getTime)
      val rnd = new Random().nextInt(10000)
      val inputName = s"web_${time}_$rnd"

      val path = Paths.get(s"model/$inputName.rpls")
      Files.write(path, checkReq.code.getBytes(StandardCharsets.UTF_8))


      val result: Result[ReplissResult] = Repliss.checkInput(checkReq.code, inputName)
      val response: JValue = result match {
        case NormalResult(result) =>
          val why3Results = result.why3Results
          val verificationResults = why3Results.map(why3Result => {
            val resState = why3Result.res match {
              case Valid() => "valid"
              case Timeout() => "timeout"
              case Unknown(s) => s"unknown ($s)"
            }
            Map(
              "proc" -> why3Result.proc,
              "resState" -> resState
            )
          })
          val verificationResultJson: Map[String, JValue] =
            Map("verificationResults" -> verificationResults)
          val counterexampleJson: Option[Map[String, JValue]] = result.counterexample.map(example =>
            Map[String, JValue]("counterexample" ->
              ("invline" -> example.brokenInvariant.start.line)
              ~ ("trace" -> example.trace)
              ~ ("svg" -> example.counterExampleSvg)
            ))

          verificationResultJson ++ counterexampleJson.getOrElse(Map())
        case ErrorResult(errors) =>
          "errors" -> errors.map((err: Repliss.Error) =>
            ("line" -> err.position.start.line)
              ~ ("column" -> err.position.start.column)
              ~ ("endline" -> err.position.stop.line)
              ~ ("endcolumn" -> err.position.stop.column)
              ~ ("message" -> err.message)
          )

      }
      Ok(response)
    })
  }

  private val logger = Logger("ReplissService")

  private val mainPage = new MainPage

  def get(request: Request): Task[Response] = {
    logger.error("Hello!!")
    Ok(mainPage.mainTemplate())
  }

  implicit def htmlEncoder: EntityEncoder[TypedTag[Predef.String]] = {
    stringEncoder.withContentType(`Content-Type`(MediaType.`text/html`)).contramap(html => s"<!DOCTYPE html>\n${html.render}")
    //    val headers = Headers(
    //      `Content-Type`(MediaType.`text/html`)
    //    )
    //    EntityEncoder.encodeBy(headers)(html => Task.delay(html.render))
  }


}

//class HtmlEncoder extends EntityEncoder[TypedTag[Predef.String]] {
//
//  /** Convert the type `A` to an [[EntityEncoder.Entity]] in the `Task` monad */
//  def toEntity(a: TypedTag[Predef.String]): Task[EntityEncoder.Entity] = {
//    Task.delay(a.render)
//  }
//
//  /** Headers that may be added to a [[Message]]
//    *
//    * Examples of such headers would be Content-Type.
//    * __NOTE:__ The Content-Length header will be generated from the resulting Entity and thus should not be added.
//    */
//  def headers: Headers
//
//}