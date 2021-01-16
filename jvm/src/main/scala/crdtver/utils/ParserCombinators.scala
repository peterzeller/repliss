package crdtver.utils

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.language.implicitConversions
import PrettyPrintDoc._
import shapeless.<:!<

/**
 * very slow and straight-forward parser generators for recursive descent
 */
object ParserCombinators {

  case class Input(
    text: String,
    position: Int
  ) {
    def peek: String = text.substring(position, Math.min(position + 15, text.length))

    def forward(length: Int): Input =
      copy(position = position + length)

  }

  sealed abstract class ParseResult[+T] {
    def map[V](f: T => V): ParseResult[V]

    def flatMap[V](f: (T, Input) => ParseResult[V]): ParseResult[V]
  }

  case class Success[T](
    result: T,
    rest: Input
  ) extends ParseResult[T] {
    override def map[V](f: T => V): ParseResult[V] = Success(f(result), rest)

    override def flatMap[V](f: (T, Input) => ParseResult[V]): ParseResult[V] =
      f(result, rest)


  }

  case class Fail(
    pos: Int,
    message: String,
  ) extends ParseResult[Nothing] {
    override def map[V](f: Nothing => V): ParseResult[V] = this

    override def flatMap[V](f: (Nothing, Input) => ParseResult[V]): ParseResult[V] = this
  }

//  var nesting = 0

  trait Parser[T] {
    outer =>
    def parseIntern(input: Input): ParseResult[T]
    
    def parse(input: Input): ParseResult[T] = {
//      println(s"${"  " * nesting} parsing ${input.peek} with ${print}")
//      nesting += 1
      val r = parseIntern(input)
//      nesting -= 1
      r
    }

    def print: PrettyPrintDoc.Doc

    override def toString: String = print.prettyStr(120)

    //    def parse(input: Input): ParseResult[T] = {
    //
    //      parseIntern(input)
    //    }

    /** sequential composition */
    final def ~[V, R](other: Parser[V])(implicit c: Combine[T, V, R]): Parser[R] =
      new Parser[R] {
        override def parseIntern(input: Input): ParseResult[R] = {
          outer.parse(input) match {
            case Success(result1, rest1) =>
              other.parse(rest1) match {
                case Success(result2, rest2) =>
                  Success(c.combine(result1, result2), rest2)
                case f: Fail => f
              }
            case f: Fail => f
          }
        }

        override def print: PrettyPrintDoc.Doc = "(" <> outer.print <+> "~" <+> other.print <> ")"
      }


    final def map[V](f: T => V): Parser[V] = new Parser[V] {

      override def parseIntern(input: Input): ParseResult[V] =
        outer.parse(input).map(f)

      override def print: PrettyPrintDoc.Doc = outer.print
    }

    @deprecated
    final def flatMap[V](f: T => Parser[V]): Parser[V] = new Parser[V] {
      override def parseIntern(input: Input): ParseResult[V] =
        outer.parse(input) match {
          case Success(result, rest) =>
            f(result).parse(rest)
          case f: Fail => f
        }

      override def print: PrettyPrintDoc.Doc = outer.print
    }

    def ^^[V](f: T => V): Parser[V] = map(f)

    def ignore: Parser[Unit] = map(_ => ())

    def ~>[V](right: Parser[V]): Parser[V] =
      this.ignore ~ right


    def <~[V](right: Parser[V]): Parser[T] =
      this ~ right.ignore


    def |(right: Parser[T]): Parser[T] = new Parser[T] {
      override def parseIntern(input: Input): ParseResult[T] = {
        outer.parse(input) match {
          case s: Success[T] => s
          case f: Fail =>
            right.parse(input) match {
              case s: Success[T] => s
              case Fail(pos, message) =>
                Fail(pos, f.message + "\n" + message)
            }
        }
      }

      override def print: PrettyPrintDoc.Doc = group("(" <> outer.print </> "|" <+> right.print <> ")")
    }


  }

  def optional[T](parser: Parser[T]): Parser[Option[T]] = new Parser[Option[T]] {
    override def parseIntern(input: Input): ParseResult[Option[T]] =
      parser.parse(input) match {
        case s: Success[T] => s.map(Some(_))
        case Fail(pos, message) =>
          Success(None, input)
      }

    override def print: Doc = "(" <> parser.print <> ")?"
  }

  def rec[T](name: String, parser: => Parser[T]): Parser[T] = new Parser[T] {
    override def parseIntern(input: Input): ParseResult[T] =
      parser.parse(input)

    override def print: Doc = name
  }

  def EOF: Parser[Unit] = new Parser[Unit] {
    override def parseIntern(input: Input): ParseResult[Unit] =
      if (input.position == input.text.length)
        Success((), input)
      else
        Fail(input.position, "Expected end of input.")

    override def print: Doc = "EOF"
  }


  def rep[T](p: Parser[T]): Parser[List[T]] = new Parser[List[T]] {
    override def parseIntern(input: Input): ParseResult[List[T]] = {
      var in = input
      val res = new ListBuffer[T]
      while (true) {
        p.parse(in) match {
          case Success(result, rest) =>
            res.addOne(result)
            in = rest
          case f: Fail =>
            return Success(res.toList, in)
        }
      }
      Fail(input.position, "Failed repeat")
    }

    override def print: Doc = group("rep(" <> nested(2, line <> p.print) <> ")")
  }


  implicit def stringParser(s: String): Parser[String] = new Parser[String] {
    override def parseIntern(input: Input): ParseResult[String] = {
      if (input.position + s.length <= input.text.length
        && input.text.subSequence(input.position, input.position + s.length) == s) {
        Success(s, input.forward(s.length))
      } else {
        Fail(input.position, s"Expected ${s} at ${input.peek}")
      }
    }

    override def print: Doc = "\"" + s+ "\""
  }

  implicit def regexParser(s: Regex): Parser[String] = new Parser[String] {
    override def parseIntern(input: Input): ParseResult[String] = {
      val m = s.findPrefixMatchOf(input.text.subSequence(input.position, input.text.length))
      m match {
        case Some(value) =>
          val matched = value.matched
          Success(matched, input.forward(matched.length))
        case None =>
          Fail(input.position, s"Expected ${s} at ${input.peek}")
      }
    }

    override def print: Doc = "\"" + s + "\".r"
  }

  sealed abstract class Combine[A, B, R] {
    def combine(a: A, b: B): R
  }

  implicit def combineUnitL[T]: Combine[Unit, T, T] = new Combine[Unit, T, T] {
    override def combine(a: Unit, b: T): T = b
  }

  implicit def combineUnitR[T]: Combine[T, Unit, T] = new Combine[T, Unit, T] {
    override def combine(a: T, b: Unit): T = a
  }

  implicit def combinePair[A, B](implicit aNoUnit: A <:!< Unit, bNoUnit: B <:!< Unit): Combine[A, B, (A, B)] = new Combine[A, B, (A, B)] {
    override def combine(a: A, b: B): (A, B) = (a, b)
  }


}
