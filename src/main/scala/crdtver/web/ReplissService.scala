package crdtver.web

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.{Calendar, Random}

import com.typesafe.scalalogging.Logger
import crdtver.Repliss._
import crdtver.symbolic.{SymbolicCounterExample, SymbolicExecutionRes}
import crdtver.{Repliss, RunArgs}
import org.http4s.{Request, _}
import org.http4s.dsl.{Ok, _}
import org.http4s.headers.`Content-Type`
import org.http4s.json4s.native._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.{JValue, JsonFormat}
import scalatags.Text.TypedTag
import scalaz.concurrent.Task

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.xml.Elem

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

      import scalaz.stream._


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
          val responseQueue = async.unboundedQueue[String]
          println("result: starting")
          responseQueue.enqueueOne("<results>").run
          println("result: starting2")
          val counterexampleFut = result.counterexampleFut.map {
            case Some(counterexample) =>
              println("result: counterexample some")
              val svg = counterexample.counterExampleSvg.replace("font-size=\"14.00\"", "font-size=\"14px\"")
              val info = counterexample.info.map(_.toString)
              val xml: Elem =
                <counterexample invline={counterexample.brokenInvariant.start.line.toString}
                                info={info.mkString("; ")}>
                  {svg}
                </counterexample>
              responseQueue.enqueueOne(xml.toString()).run
            case None =>
              println("result: counterexample none")
              responseQueue.enqueueOne(s"<nocounterexample />").run
            // TODO
          }

          val symbolicFut = Future {
            result.symbolicExecutionResultStream.foreach((sResult: SymbolicExecutionRes) => {
              println(s"result: symbolic $sResult")
              val verificationDetails: ListBuffer[Elem] = ListBuffer()
              val resState = sResult.error match {
                case Some(verificationError) =>
                  "failed"
                case None =>
                  "valid"
              }

              sResult.error.foreach((verificationError: SymbolicCounterExample) => {
                verificationDetails += <message>
                  {verificationError.message}
                </message>
                verificationDetails += <location line={verificationError.errorLocation.start.line.toString}/>
                verificationDetails += <isabelleTranslation>
                  {verificationError.isabelleTranslation}
                </isabelleTranslation>
                verificationDetails += <smtTranslation>
                  {verificationError.smtTranslation}
                </smtTranslation>
                verificationDetails += <trace>
                  {for (step <- verificationError.trace.steps) yield {
                    <step description={step.description} blub={step.source.getLine.toString}>

                    </step>
                  }}
                </trace>
                //                message: String,
                //                errorLocation: SourceRange,
                //                // trace including a model for the state after each step
                //                trace: Trace[Option[SymbolicCounterExampleModel]],
                //                model: Option[SymbolicCounterExampleModel],
                //                isabelleTranslation: String,
                //                smtTranslation: String

                verificationError.model match {
                  case Some(model) =>
                    val svg = model.counterExampleSvg
                    val xml: Elem =
                      <counterexample invline={verificationError.errorLocation.start.line.toString}
                                      info={model.modelText.prettyStr(120).replace('\n', ';')}>
                        {svg}
                      </counterexample>
                    verificationDetails += xml
                  case None =>
                      <counterexample invline={verificationError.errorLocation.start.line.toString}
                                      info={verificationError.message.replace('\n', ';')}/>
                }
              })

              val xml = <verificationResult
              proc={sResult.proc}
              resState={resState}>
                verificationDetails
              </verificationResult>
              responseQueue.enqueueOne(xml.toString()).run
            })

          }

          for (_ <- symbolicFut; _ <- counterexampleFut) yield {
            responseQueue.enqueueOne("</results>").run
            responseQueue.close.run
          }


          Ok(responseQueue.dequeue).withContentType(Some(textXml))
        case ErrorResult(errors) =>
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

          Ok(response.toString()).withContentType(Some(textXml))

      }

    })
  }


  val textXml: `Content-Type` = `Content-Type`(MediaType.`text/xml`, Charset.`UTF-8`)
  private val logger = Logger("ReplissService")

  private val mainPage = new MainPage

  def get(request: Request): Task[Response] = {
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