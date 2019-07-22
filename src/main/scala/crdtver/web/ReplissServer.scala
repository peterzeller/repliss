package crdtver.web

import java.util.concurrent.{Executors, ScheduledExecutorService}

import com.typesafe.scalalogging.Logger
import crdtver.RunArgs
import crdtver.utils.Helper
import org.http4s.{HttpRoutes, _}
import org.http4s.headers._
import org.http4s.server.blaze._
import org.http4s.server.{Router, Server}
import scodec.bits.ByteVector

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import cats.effect._
import crdtver.utils.ConcurrencyUtils.Task
import org.http4s._
import org.http4s.dsl.io._
import org.http4s._
import org.http4s.dsl.io._
import java.io.File

import cats.implicits._
// import cats.implicits._

import org.http4s.server.blaze._
// import org.http4s.server.blaze._

import org.http4s.implicits._
// import org.http4s.implicits._

import org.http4s.server.Router

import scala.concurrent.ExecutionContext.Implicits.global


object ReplissServer extends IOApp {

//  implicit val cs: ContextShift[IO] = IO.contextShift(global)
//  implicit val timer: Timer[IO] = IO.timer(global)
  private val blockingPool = Executors.newFixedThreadPool(4)
  private val blocker = Blocker.liftExecutorService(blockingPool)

  def run(args: List[String]): IO[ExitCode] = {
    val runArgs: RunArgs = RunArgs.parse(args).get


    val httpApp = Router(
      "/" -> staticFiles(""),
      "/webjars/" -> staticFiles("/META-INF/resources/webjars"),
      "/api" -> service,
      "/" -> indexPage
    ).orNotFound

    BlazeServerBuilder[IO]
      .withIdleTimeout(Duration.Inf)
      .bindHttp(runArgs.port, runArgs.host)
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  private val indexPage = HttpRoutes.of[IO] {
    case request@GET -> Root =>
      StaticFile.fromResource("/html/index.html", blocker, Some(request)).getOrElseF(NotFound())
    case request@GET -> Root / "webjars" / "ace" / "01.08.2014" / "src-noconflict" / "mode-repliss.js" =>
      StaticFile.fromResource("/js/mode-repliss.js", blocker, Some(request)).getOrElseF(NotFound())

  }

//  def debugEncode[A](implicit W: EntityEncoder[A]): EntityEncoder[Process[Task, A]] =
//    new EntityEncoder[Process[Task, A]] {
//      override def toEntity(a: Process[Task, A]): Task[Entity] = {
//        println("toEntity start")
//        val p: Process[Task, ByteVector] = a.flatMap(elem => {
//          println(s"toEntity receiving $elem")
//          Process.await(W.toEntity(elem))(_.body)
//        })
//        println("toEntity done")
//        Task.now(Entity(p, None))
//      }
//
//      override def headers: Headers =
//        W.headers.get(`Transfer-Encoding`) match {
//          case Some(transferCoding) if transferCoding.hasChunked =>
//            W.headers
//          case _ =>
//            W.headers.put(`Transfer-Encoding`(TransferCoding.chunked))
//        }
//    }

  private val logger = Logger("ReplissServer")

  private def notFound: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case _ =>
      NotFound("Page not found.")
  }

  private def static(file: String, request: Request[IO]): IO[Response[IO]] = {
    logger.trace(s"serving $file")
    if (file.endsWith("mode-repliss.js")) {
      return StaticFile.fromResource("/js/mode-repliss.js", blocker, Some(request)).getOrElseF(NotFound())
    }
    StaticFile.fromResource("/META-INF/resources" + file, blocker, Some(request)).getOrElseF {
      StaticFile.fromResource(file, blocker, Some(request)).getOrElseF(NotFound(s"File $file not found."))
    }
  }

  private def staticFiles(prefix: String): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case request@GET -> path if List(".js", ".css", ".map", ".html", ".webm", ".svg").exists(path.toString.endsWith) =>
      logger.trace(s"Serving static file $path")
      static(prefix + path.toString, request)
  }


  private val service: HttpRoutes[IO] = {
    val replissSevice = new ReplissService

    HttpRoutes.of[IO] {
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

  private def exampleJs(): IO[Response[IO]] = {
    val examples = List(
      ReplissExample("Userbase", "verified/userbase.rpls"),
      ReplissExample("Userbase (missing transaction)", "failsToVerify/userbase_fail1.rpls"),
      ReplissExample("Userbase (wrong datatype)", "failsToVerify/userbase_fail2.rpls"),
      ReplissExample("Chat", "verified/chatapp.rpls"),
      ReplissExample("Chat (Bug)", "failsToVerify/chatapp_fail1.rpls"),
//      ReplissExample("Userbase2", "wip/userbase2.rpls"),
//      ReplissExample("Userbase (missing transaction)", "wip/userbase_fail1.rpls"),
//      ReplissExample("Userbase (wrong CRDT)", "wip/userbase_fail2.rpls"),
//      ReplissExample("Friends", "wip/friends.rpls"),
//      ReplissExample("Friends 2", "wip/friends2.rpls"),
//      ReplissExample("Tournament (WIP)", "wip/tournament.rpls"),
//      ReplissExample("Singleton set (bug)", "wip/singleton_set.rpls"),
//      ReplissExample("Active view (bug)", "wip/active_view.rpls"),
//      ReplissExample("MoveTask", "wip/task2.rpls")
    )

    import org.json4s._
    import org.json4s.native.Serialization
    import org.json4s.native.Serialization.write
    implicit val formats = Serialization.formats(NoTypeHints)

    val examplesWithCode =
      for (ex <- examples) yield
        ex.copy(code = Helper.getResource(s"/examples/${ex.file}"))

    val json: String = write(examplesWithCode)

//    Response(status = Status.Ok, headers = Headers.of(org.http4s.headers.`Access-Control-Allow-Origin`: "*"), body = EntityEncoder.stringEncoder.toEntity(json))

    Ok(json,  Header("Access-Control-Allow-Origin", "*")) // .withType(MediaType.`application/json`)
  }


}
