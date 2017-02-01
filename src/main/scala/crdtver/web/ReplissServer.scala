package crdtver.web

import com.typesafe.scalalogging.Logger
import crdtver.{Helper, RunArgs}
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze._
import org.http4s.server.{Server, ServerApp}

import scalaz.concurrent.Task

object ReplissServer extends ServerApp {


  override def server(args: List[String]): Task[Server] = {
    val runArgs: RunArgs = RunArgs.parse(args).get
    BlazeBuilder
      .bindHttp(runArgs.port, runArgs.host)
      .mountService(staticFiles(""), "/")
      .mountService(staticFiles("/META-INF/resources/webjars"), "/webjars/")
      .mountService(service, "/api")
      .mountService(indexPage, "/")
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
      case request@GET -> Root / "examples" =>
        exampleJs()
    }
  }

  case class ReplissExample(
    name: String,
    file: String,
    code: String = "",
    description: String = ""
  )

  private def exampleJs(): Task[Response] = {
    val examples = List(
      ReplissExample("Userbase", "userbase.rpls"),
      ReplissExample("Userbase (missing transaction)", "userbase_fail1.rpls"),
      ReplissExample("Userbase (wrong CRDT)", "userbase_fail2.rpls"),
      ReplissExample("Friends", "friends.rpls"),
      ReplissExample("Friends 2", "friends2.rpls")
    )

    import org.json4s._
    import org.json4s.native.Serialization
    import org.json4s.native.Serialization.write
    implicit val formats = Serialization.formats(NoTypeHints)

    val examplesWithCode =
      for (ex <- examples) yield
        ex.copy(code = Helper.getResource(s"/examples/${ex.file}"))

    val json: String = write(examplesWithCode)

    Ok(json).withType(MediaType.`application/json`)

  }


}
