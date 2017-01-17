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
      .mountService(indexPage, "/")
      .mountService(staticFiles("/META-INF/resources/webjars"), "/webjars/")
      .mountService(staticFiles(""), "/")
      .mountService(service, "/api")
      .start
  }

  private val indexPage = HttpService {
    case request@GET -> Root =>
      StaticFile.fromResource("/html/index.html").map(Task.now).get
    case request@GET -> Root / "webjars" / "ace" / "01.08.2014" / "src-noconflict" / "mode-repliss.js" =>
      StaticFile.fromResource("/js/mode-repliss.js").map(Task.now).get
  }

  private val logger = Logger("ReplissServer")

  private def notFound: HttpService = HttpService {
    case _ =>
      NotFound("Page not found.")
  }

  private def static(file: String, request: Request): Task[Response] = {
    logger.error(s"serving $file")
    if (file.endsWith("mode-repliss.js")) {
      return StaticFile.fromResource("/js/mode-repliss.js").map(Task.now).get
    }
    StaticFile.fromResource("/META-INF/resources" + file, Some(request)).map(Task.now).getOrElse {
      StaticFile.fromResource(file, Some(request)).map(Task.now).getOrElse(NotFound(s"File $file not found."))
    }
  }

  private def staticFiles(prefix: String): HttpService = HttpService {
    case request@GET -> path if List(".js", ".css", ".map", ".html", ".webm", ".svg").exists(path.toString.endsWith) =>
      logger.error(s"Serving static file $path")
      static(prefix + path.toString, request)
  }


  private val service: HttpService = {
    val replissSevice = new ReplissService

    HttpService {
      case request@POST -> Root / "check" =>
        replissSevice.check(request)
    }
  }


}
