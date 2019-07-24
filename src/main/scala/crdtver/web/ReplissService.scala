package crdtver.web

import io.circe.syntax._
import io.circe.generic.auto._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.{Calendar, Random}

import cats.effect.IO
import cats.effect.syntax.async
import com.typesafe.scalalogging.Logger
import crdtver.{Repliss, RunArgs}
import crdtver.Repliss._
import org.http4s.Request
import org.http4s.headers.`Content-Type`
import org.json4s.JsonAST._
import org.json4s.{JValue, JsonFormat}
import org.json4s.native.JsonMethods._
import scalatags.Text.TypedTag
import org.json4s.JsonDSL._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.xml.Elem
import cats.effect._
import crdtver.utils.StreamUtils
import io.circe.Encoder
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.client._
import org.http4s.client.dsl.io._
import cats.effect._
import crdtver.symbolic.{SymbolicCounterExample, SymbolicCounterExampleModel, SymbolicExecutionRes}
import io.circe._
import io.circe.literal._
import org.http4s._
import org.http4s.dsl.io._

import scala.collection.mutable.ListBuffer


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

  implicit val CheckRequestEncoder: Encoder[CheckRequest] =
    Encoder.instance { obj: CheckRequest =>
      Json.obj(
        "code" -> Json.fromString(obj.code)
      )
    }

  implicit val CheckRequestDecoder: Decoder[CheckRequest] =
    Decoder.instance { json: HCursor =>
      for {
        code <- json.downField("code").as[String]
      } yield CheckRequest(code)
    }

  def check(request: Request[IO]): IO[Response[IO]] = {
//    implicit val decoder: EntityDecoder[IO, CheckRequest] = new EntityDecoder[IO, CheckRequest] {
//      override def decode(msg: Message[IO], strict: Boolean): DecodeResult[IO, CheckRequest] = ???
//
//      override def consumes: Set[MediaRange] = ???
//    }
    implicit val checkRequestDcoder: EntityDecoder[IO, CheckRequest] = org.http4s.circe.jsonOf[IO, CheckRequest]

    request.as[CheckRequest].flatMap((checkReq: CheckRequest) => {


      val format = new SimpleDateFormat("yyyy-MM-dd'T'HHmmss-SSS")
      val time = format.format(Calendar.getInstance().getTime)
      val rnd = new Random().nextInt(10000)
      val inputName = s"web_${time}_$rnd"

      val path = Paths.get(s"model/$inputName.rpls")
      Files.write(path, checkReq.code.getBytes(StandardCharsets.UTF_8))


      val result: Result[ReplissResult] = Repliss.checkInput(checkReq.code, inputName, checks = List(SymbolicCheck(), Quickcheck()), runArgs = RunArgs())

      result match {
        case NormalResult(result) =>
          //          val why3Results = result.why3Results
          //          val verificationResults = why3Results.map(why3Result => {
          //            val resState = why3Result.res match {
          //              case Valid() => "valid"
          //              case Timeout() => "timeout"
          //              case Unknown(s) => s"unknown ($s)"
          //            }
          //            Map(
          //              "proc" -> why3Result.proc,
          //              "resState" -> resState
          //            )
          //          })
          //          val verificationResultJson: Map[String, JValue] =
          //            Map("verificationResults" -> verificationResults)
          //          val counterexampleJson: Option[Map[String, JValue]] = result.counterexample.map(example =>
          //            Map[String, JValue]("counterexample" ->
          //              ("invline" -> example.brokenInvariant.start.line)
          //              ~ ("trace" -> example.trace)
          //              ~ ("svg" -> example.counterExampleSvg)
          //            ))
          //
          //          verificationResultJson ++ counterexampleJson.getOrElse(Map())
          //
          import scala.concurrent.ExecutionContext.Implicits.global

          implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)


          val counterExampleStream: fs2.Stream[IO, String] = fs2.Stream.eval(IO.fromFuture(IO {
            result.counterexampleFut.map {
              case Some(counterexample) =>
                val info = counterexample.info.map(_.toString.replace("\n", ";"))
                val xml: Elem =
                  <counterexample invline={counterexample.brokenInvariant.start.line.toString}
                                  info={info.mkString("; ")}>
                    {counterexample.counterExampleRender.toXml}
                  </counterexample>
                xml.toString()
              case None =>
                s"<nocounterexample />"
            }
          }))


          val verificaionResultStream: fs2.Stream[IO, String] =
            StreamUtils.fromLazyListIO(result.symbolicExecutionResultStream).evalMap((why3Result: SymbolicExecutionRes) => IO {
              val details = ListBuffer[Elem]()

              for (t <- why3Result.translations) {
                details.addOne(t.toXml)
              }

              def counterExampleToXml(o: Option[SymbolicCounterExampleModel]): Elem = o match {
                case Some(ce) =>
                  ce.toXml
                case None =>
                    <noCounterExample />
              }

              why3Result.error.foreach((error: SymbolicCounterExample) => {
                details.addOne(
                  <error
                  message={error.message}
                  errorLocation={error.errorLocation.toString}
                  >
                    {error.trace.toXml(counterExampleToXml)}
                    {for (m <- error.model) yield m.toXml}
                    {error.translation.toXml}
                  </error>)
                /*
                message: String,
                  errorLocation: SourceRange,
                  // trace including a model for the state after each step
                  trace: Trace[Option[SymbolicCounterExampleModel]],
                  model: Option[SymbolicCounterExampleModel],
                  translation: Translation
                 */


              })


              val resState = why3Result.error match {
                case None => "valid"
                case Some(error) =>
                  error.model match {
                    case Some(counterExample) =>
                      "error"
                    case None =>
                     s"unknown"
                  }
              }
              val xml = <verificationResult
                proc={why3Result.proc}
                time={why3Result.time.toString}
                resState={resState}>
                {details}
              </verificationResult>
              fs2.Stream.emit(xml.toString())
            }).flatMap(x => x)


          val procedures =
            <procedures>
              {for (p <- result.typedProgram.procedures) yield
                <procedure name={p.name.name} />}
            </procedures>

          val resultStream: fs2.Stream[IO, String] =
            fs2.Stream.emit("<results>") ++
              fs2.Stream.emit(procedures.toString()) ++
              (verificaionResultStream merge counterExampleStream) ++
              fs2.Stream.emit("</results>")


          Ok(resultStream, Header("Access-Control-Allow-Origin", "*")) // TODO .withContentType(Some(textXml))
        case ErrorResult(errors)
        =>
          val response =
            <results>
              {for (err <- errors) yield
                <error
                line={err.position.start.line.toString}
                column={err.position.start.column.toString}
                endline={err.position.stop.line.toString}
                endcolumn={err.position.stop.column.toString}
                message={err.message}/>}
            </results>

          Ok(response.toString(), Header("Access-Control-Allow-Origin", "*"))  // TODO .withContentType(Some(textXml))

      }

    }

    )
  }


  val textXml: `Content-Type` = `Content-Type`(MediaType.unsafeParse("text/xml"), Charset.`UTF-8`)
  private val logger = Logger("ReplissService")

  private val mainPage = new MainPage

  def get(request: Request[IO]): IO[Response[IO]] = {
    Ok(mainPage.mainTemplate())
  }

  implicit def htmlEncoder: EntityEncoder[IO, TypedTag[Predef.String]] = {
    EntityEncoder.stringEncoder[IO]
      .withContentType(`Content-Type`(MediaType.unsafeParse("text/html")))
      .contramap(html => s"<!DOCTYPE html>\n$html")
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
