package crdtver.web

import com.typesafe.scalalogging.Logger
import org.http4s._
import org.http4s.dsl._

import scalaz.concurrent.Task
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze._
import org.http4s.server.blaze._
import org.http4s.server.syntax._

import scalaz.Kleisli

object ReplissServer extends ServerApp {
  override def server(args: List[String]): Task[Server] = {
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(services, "/")
      .start
  }

  private val logger = Logger("ReplissServer")

  private def services = service orElse staticFiles orElse notFound

  private def notFound: HttpService = HttpService {
    case _ =>
      NotFound("Page not found.")
  }

  private def static(file: String, request: Request): Task[Response] = {
    logger.error(s"serving $file")
    StaticFile.fromResource("/META-INF/resources" + file, Some(request)).map(Task.now).getOrElse(NotFound())
  }

  private val staticFiles: HttpService = HttpService {
    case request@GET -> path if List(".js", ".css", ".map", ".html", ".webm").exists(path.toString.endsWith) =>
      logger.error(s"Serving static file $path")
      static(path.toString, request)
  }


  private val service: HttpService = {
    val replissSevice = new ReplissService

    HttpService {
      case request@GET -> Root =>
        replissSevice.get(request)
    }
  }


}
