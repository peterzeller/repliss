package repliss.js


import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.Dynamic

//@js.native
//@JSGlobal
//object JSON extends js.Object {
//  def parse(text: String): js.Any = js.native
//
//  def stringify(value: js.Any): String = js.native
//}

object Json {
  type Result[T] = Either[ParseError, T]


  def decode[T](v: String)(implicit decoder: Decoder[T]): Result[T] =
    decoder.decode(JSON.parse(v))



  case class ParseError(message: String) extends java.lang.Error

  trait Decoder[T] {
    def decode(value: js.Dynamic): Either[ParseError, T]
  }


  implicit def encodeList[T](implicit decoder: Decoder[T]): Decoder[List[T]] =
    new Decoder[List[T]] {
      override def decode(value: js.Dynamic): Either[ParseError, List[T]] = {
        val list: js.Array[js.Dynamic] = value.asInstanceOf[js.Array[js.Dynamic]]
        Console.println("Decoded list", list)
        val r = for {
          x <- list.toList
        } yield decoder.decode(x) match {
          case Left(err) =>
            return Left(err)
          case Right(value) =>
            value
        }
        Right(r)
      }
    }

  implicit def decodeExample: Decoder[Data.Example] =
    new Decoder[Data.Example] {
      override def decode(value: js.Dynamic): Either[ParseError, Data.Example] = {
        Console.println("Decode example", JSON.stringify(value))
        Right(Data.Example(value.name.asInstanceOf[String], value.code.asInstanceOf[String]))
      }
    }



}
