package crdtver.web

import com.typesafe.scalalogging.Logger
import org.http4s.Request
import org.http4s.dsl.Ok
import org.http4s._
import org.http4s.dsl._
import org.http4s.headers.`Content-Type`

import scalaz.concurrent.Task
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze._

import scalatags.Text.TypedTag


class ReplissService {
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