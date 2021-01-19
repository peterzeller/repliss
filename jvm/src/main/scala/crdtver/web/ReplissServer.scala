package crdtver.web

import java.io.{File, InputStreamReader}
import java.util.Arrays.asList
import java.util.concurrent.Executors
import cats.effect._
import cats.implicits._
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.ext.toc.internal.TocOptions
import com.vladsch.flexmark.util.options.MutableDataSet
import crdtver.RunArgs
import crdtver.utils.{Helper, ReplissVersion}
import org.http4s.{HttpRoutes, _}
import org.http4s.dsl.io._
import org.http4s.headers._
import org.log4s.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
// import cats.implicits._

import org.http4s.server.blaze._
// import org.http4s.server.blaze._

import org.http4s.implicits._
// import org.http4s.implicits._

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import org.http4s.server.Router

import cats.implicits._
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.Router

object ReplissServer extends IOApp {

  //  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  //  implicit val timer: Timer[IO] = IO.timer(global)
  private val blockingPool = Executors.newFixedThreadPool(4)
  private val blocker = Blocker.liftExecutorService(blockingPool)

  def run(args: List[String]): IO[ExitCode] = {
    val runArgs: RunArgs = RunArgs.parse(args).get


    val httpApp = Router(
      "/scalajs/" -> HttpRoutes.of[IO](scalaJsMatcher),
      "/css/" -> staticFiles("/css/"),
      "/js/" -> staticFiles("/js/"),
//      "/" -> staticFiles("META-INF/resources/webjars/replissjvm/0.1.0-SNAPSHOT/"),
//      "/" -> staticFiles("jvm/target/web/classes/main/META-INF/resources/webjars/replissjvm/0.1.0-SNAPSHOT"),
//      "/style.css" -> StaticFile.fromResource("/public/"),
      "/webjars/" -> staticFiles("/META-INF/resources/webjars"),
      "/api" -> service,
      "/" -> indexPage,
      "/docs" -> documentationPage
    ).orNotFound

    BlazeServerBuilder[IO](executionContext = global)
      .withIdleTimeout(Duration.Inf)
      .bindHttp(runArgs.port, runArgs.host)
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  private val indexPage = HttpRoutes.of[IO](indexPageMatcher)

  type Matcher = PartialFunction[Request[IO], IO[Response[IO]]]

  private def indexPageMatcher: Matcher = {
    case request@GET -> Root =>
      StaticFile.fromResource("/html/index.html", blocker, Some(request)).getOrElseF(NotFound())
  }

  private def scalaJsMatcher: Matcher = {
    case request@GET -> Root / "repliss.js" =>
      logger.trace(s"Getting repliss.js ${request.uri}")
      StaticFile.fromResource("META-INF/resources/webjars/replissjvm/0.1.0-SNAPSHOT/replissjs-fastopt-bundle.js", blocker, Some(request))
        .getOrElseF(
          StaticFile.fromFile(new File("jvm/target/web/classes/main/META-INF/resources/webjars/replissjvm/0.1.0-SNAPSHOT/replissjs-fastopt-bundle.js"), blocker, Some(request))
            .getOrElseF(NotFound()))
    case req =>
      logger.trace(s"Getting scalajs somewhere else: ${req.uri}")
      NotFound()
  }

  private def aceModeReplissMatcher: Matcher = {
    case request@GET -> Root / "webjars" / "ace" / "01.08.2014" / "src-noconflict" / "mode-repliss.js" =>
      StaticFile.fromResource("/js/mode-repliss.js", blocker, Some(request)).getOrElseF(NotFound())
  }


  private val documentationPage = HttpRoutes.of[IO] {
    case request@GET -> Root =>
      renderDocumentationPage

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

  private def renderDocumentationPage = {
    val loader = Thread.currentThread.getContextClassLoader

    val template: String = Source.fromResource("html/template.html").mkString

    val textTry: Try[String] = Using(loader.getResourceAsStream("doc.md")) { is =>


      val options: MutableDataSet = new MutableDataSet
      options.set(Parser.EXTENSIONS, asList(
        TocExtension.create(),
        AnchorLinkExtension.create()
      ))
      options.set[Integer](TocExtension.LEVELS, TocOptions.getLevels(1, 2, 3))
      options.set[java.lang.Boolean](HtmlRenderer.GENERATE_HEADER_ID, true)
      options.set[java.lang.Boolean](AnchorLinkExtension.ANCHORLINKS_WRAP_TEXT, false)
      options.set(AnchorLinkExtension.ANCHORLINKS_ANCHOR_CLASS, "anchor-link")


      val parser = Parser.builder(options).build
      val renderer = HtmlRenderer.builder(options).build
      val document = parser.parseReader(new InputStreamReader(is))
      val html = renderer.render(document)

      template.replace("""<div id="body"></div>""", html)
    }

    textTry match {
      case Failure(exception) =>
        exception.printStackTrace()
        InternalServerError(exception.getMessage)
      case Success(text) =>
        Ok(text, `Content-Type`(MediaType.text.html))
    }


  }

  private val logger: Logger = org.log4s.getLogger("ReplissServer")

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

    val check: Matcher = {
      case request@POST -> Root / "check" =>
              replissSevice.check(request)
    }

    val examples: Matcher = {
      case GET -> Root / "examples" =>
              exampleJs()
    }

    val version: Matcher = {
      case GET -> Root / "version" =>
              getVersion()
    }

    HttpRoutes.of[IO] (check orElse examples orElse version)
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
      ReplissExample("Userbase (missing transaction)", "buggy/userbase_fail1.rpls"),
      ReplissExample("Userbase (wrong datatype)", "buggy/userbase_fail2.rpls"),
      ReplissExample("Chat", "verified/chatapp.rpls"),
      ReplissExample("Chat (Bug)", "buggy/chatapp_fail1.rpls"),
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
    implicit val formats: Formats = Serialization.formats(NoTypeHints)

    val examplesWithCode =
      for (ex <- examples) yield
        ex.copy(code = Helper.getResource(s"/examples/${ex.file}"))

    val json: String = write(examplesWithCode)

    Ok(json, Header("Access-Control-Allow-Origin", "*")) // .withType(MediaType.`application/json`)
  }

  private def getVersion(): IO[Response[IO]] = {
    val v = ReplissVersion.version


    import org.json4s._
    import org.json4s.native.Serialization
    import org.json4s.native.Serialization.write
    implicit val formats: Formats with Formats = Serialization.formats(NoTypeHints)
    val json: String = write(v)
    Ok(json, Header("Access-Control-Allow-Origin", "*"))
  }


}
