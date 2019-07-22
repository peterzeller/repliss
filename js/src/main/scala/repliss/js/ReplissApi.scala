package repliss.js


import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

object ReplissApi {

  def getExamples: Future[Either[Error, List[Data.Example]]] = {
    Console.println(s"Getting examples")
    val request = HttpRequest("//localhost:8080/api/examples")
    request.send().map(r => {
      Console.println(s"Got examples: ${r.body}")
      val res = Json.decode[List[Data.Example]](r.body)
      Console.println(s"Parsed examples: $res")
      res
    })



  }


}