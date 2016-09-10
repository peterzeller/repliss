//package crdtver
//
//import crdtver.PrettyRenderer2.{LazyBox, LazySEmpty, LazySText, LazySimpleDoc}
//import gnieh.pp._
//
//import scala.annotation.tailrec
//import scala.collection.mutable
//
//
//object PrettyRenderer2 {
//  class LazyBox[T](private val computation: () => T) {
//    var value: Option[T] = None
//
//    def apply(): T = {
//      if (value.isEmpty) {
//        value = Some(computation())
//      }
//      value.get
//    }
//
//  }
//
//  def LazyBox[T](computation: () => T): LazyBox[T] = new LazyBox[T](computation)
//
//  sealed trait LazySimpleDoc {
//
//    def fits(width: Int): Boolean
//
//    def layout: String
//
//    override def toString = layout
//
//  }
//
//  /** An empty document.
//    */
//  case object LazySEmpty extends LazySimpleDoc {
//    def fits(width: Int) =
//      width >= 0 // always fits if there is enough place
//
//    val layout =
//      ""
//  }
//
//  /** A text document. Should never contain new lines
//    */
//  final case class LazySText(text: String, next: LazyBox[LazySimpleDoc]) extends LazySimpleDoc {
//    def fits(width: Int) = {
//      text.length < width && next().fits(width - text.length)
//    }
//
//    lazy val layout =
//      text + next().layout
//  }
//
//  /** A new line document with the indentation level to print right after.
//    *  If the next document is empty, the indentation is not printed.
//    */
//  final case class LazySLine(indent: Int, next: LazyBox[LazySimpleDoc]) extends LazySimpleDoc {
//    def fits(width: Int) =
//      width >= 0 // always fits if there is enough place
//
//    lazy val layout = {
//      val nextLayout: String = next().layout
//      if (nextLayout.isEmpty)
//        ""
//      else
//        "\n" + (" " * indent) + nextLayout
//    }
//  }
//
//
//  @tailrec
//  def printToStringBuilder(doc: SimpleDoc, sb: StringBuilder): Unit = doc match {
//    case SEmpty =>
//    case SText(text, next) =>
//      sb.append(text)
//      printToStringBuilder(next, sb)
//    case SLine(indent, next) =>
//      sb.append("\n" + (" " * indent))
//      printToStringBuilder(next, sb)
//  }
//
//  def print(doc: SimpleDoc): String = {
//    val sb = new StringBuilder()
//    printToStringBuilder(doc, sb)
//    sb.toString()
//  }
//}
//
//class PrettyRenderer2(width: Int) extends (Doc => SimpleDoc) {
//
//  private type Docs = List[(Int, Doc)]
//
//
//
//
//  def apply(doc: Doc): SimpleDoc = {
//    best5(width, 0, List((0, doc)))
//  }
//
//  import crdtver.PrettyRenderer2._
//
//
//  def best5(width: Int, column: Int, docs: Docs): SimpleDoc = {
//    var ldoc = bestLazy(width, column, docs)
//    var parts = mutable.Stack[Part]()
//    while (ldoc != LazySEmpty) {
//      ldoc match {
//        case LazySEmpty =>
//        case LazySText(text, next) =>
//          parts.push(PartText(text))
//          ldoc = next()
//        case LazySLine(indent, next) =>
//          parts.push(PartLine(indent))
//          ldoc = next()
//      }
//    }
//    makeDoc(parts)
//  }
//
//  private def bestLazy(width: Int, column: Int, docs: Docs): LazySimpleDoc = docs match {
//    case Nil =>
//      LazySEmpty
//    case (_, EmptyDoc) :: tail =>
//      bestLazy(width, column, tail)
//    case (i, ConsDoc(first, second)) :: tail =>
//      bestLazy(width, column, (i, first) :: (i, second) :: tail)
//    case (i, NestDoc(j, inner)) :: tail =>
//      bestLazy(width, column, (i + j, inner) :: tail)
//    case (i, TextDoc(text)) :: tail =>
//      LazySText(text, LazyBox(() => bestLazy(width, column + text.length, tail)))
//    case (i, LineDoc(_)) :: tail =>
//      LazySLine(i, LazyBox(() => bestLazy(width, i, tail)))
//    case (i, UnionDoc(l, s)) :: tail =>
//      betterLazy(width, column,
//        bestLazy(width, column, (i, l) :: tail),
//        () => bestLazy(width, column, (i, s) :: tail))
//    case (i, AlignDoc(inner)) :: tail =>
//      bestLazy(width, column, (column, inner) :: tail)
//    case (i, ColumnDoc(f)) :: tail =>
//      bestLazy(width, column, (i, f(column)) :: tail)
//  }
//
//  private def betterLazy(width: Int, column: Int, d1: LazySimpleDoc, d2: () => LazySimpleDoc): LazySimpleDoc =
//    if (d1.fits(width - column))
//      d1
//    else
//    // d2 is computed only if needed...
//      d2()
//
//
//
//  sealed abstract class Call
//  case class CallBest(width: Int, column: Int, docs: Docs) extends Call
//  case class CallReturn(doc: SimpleDoc) extends Call
//
//  sealed abstract class Part
//  case class PartText(txt: String) extends Part
//  case class PartLine(indent: Int) extends Part
//
//
//  var depth = 0
//
//  def pushDoc(doc: SimpleDoc, result: mutable.Stack[Part]): Unit = {
//    var d = doc
//    while (true) {
//      d match {
//        case SEmpty =>
//          return
//        case SText(text, next) =>
//          result.push(PartText(text))
//          d = next
//        case SLine(indent, next) =>
//          result.push(PartLine(indent))
//          d = next
//      }
//    }
//
//  }
//
//
//  def getTexts(count: Int, docs: Docs): String = {
//    if (count <= 0) ""
//    else docs match {
//      case Nil =>
//        ""
//      //              stack.push(CallReturn(SEmpty))
//      case (i,x) :: xs =>
//        x match {
//          case NestDoc(indent, inner) =>
//            getTexts(count, (0,inner) :: xs)
//          case UnionDoc(long, short) =>
//            getTexts(count, (0,long) :: xs)
//          case EmptyDoc =>
//            ""
//          case TextDoc(text) =>
//            text + "" + getTexts(count-1, xs)
//          case LineDoc(repl) =>
//            " |NL| " + getTexts(count-1, xs)
//          case ConsDoc(first, second) =>
//            getTexts(count, (0,first) :: (0,second) :: xs)
//          case AlignDoc(inner) =>
//            getTexts(count, (0,inner) :: xs)
//          case ColumnDoc(f) =>
//            s" |column$f| " + getTexts(count-1, xs)
//        }
//    }
//
//  }
//
//  private def best3(pwidth: Int, pcolumn: Int, pdocs: Docs): SimpleDoc = {
//
//    var stack = mutable.Stack[Call]()
//    stack.push(CallBest(pwidth, pcolumn, pdocs))
//
//    var result = mutable.Stack[Part]()
//
//    while (stack.nonEmpty) {
////      println(s"stack size = ${stack.size}, depth = $depth")
//      val elem: Call = stack.pop()
//
//      elem match {
//        case CallReturn(d) =>
////          break
//        case CallBest(width, column, docs) =>
//          docs match {
//            case Nil =>
////              stack.push(CallReturn(SEmpty))
//            case (_, EmptyDoc) :: tail =>
//              stack.push(CallBest(width, column, tail))
//            case (i, ConsDoc(first, second)) :: tail =>
//              stack.push(CallBest(width, column, (i, first) :: (i, second) :: tail))
//            case (i, NestDoc(j, inner)) :: tail =>
//              stack.push(CallBest(width, column, (i + j, inner) :: tail))
//            case (i, TextDoc(text)) :: tail =>
//              result.push(PartText(text))
//              stack.push(CallBest(width, column + text.length, tail))
//            case (i, LineDoc(_)) :: tail =>
//              result.push(PartLine(i))
//              stack.push(CallBest(width, i, tail))
//            case (i, UnionDoc(l, s)) :: tail =>
//              println(s"union $depth")
//              println(getTexts(10, docs))
////              stack.push(CallBest(width, column, (i, l) :: tail))
//              depth += 1
//              val doc = better(width, column,
//                best3(width, column, (i, l) :: tail),
//                () => {
//                  println("NO FIT!!!")
//                  println(getTexts(10, docs))
//                  best3(width, column, (i, s) :: tail)
//                })
//
//              pushDoc(doc, result)
//
//              depth -= 1
//            case (i, AlignDoc(inner)) :: tail =>
//              println("align")
//              stack.push(CallBest(width, column, (column, inner) :: tail))
//            case (i, ColumnDoc(f)) :: tail =>
//              println("column")
//              stack.push(CallBest(width, column, (i, f(column)) :: tail))
//          }
//      }
//    }
//    println("making doc")
//    makeDoc(result)
//  }
//
//
//  def makeDoc(result: mutable.Stack[Part]): SimpleDoc = {
//    var res: SimpleDoc = SEmpty
//    for (part <- result) {
//      part match {
//        case PartText(txt) =>
//          res = SText(txt, res)
//        case PartLine(i) =>
//          res = SLine(i, res)
//      }
//    }
//    res
//    //val it = result.reverseIterator
////    val it = result.iterator
////    var res: SimpleDoc = SEmpty
////    while (it.hasNext) {
////      it.next() match {
////        case PartText(txt) =>
////          res = SText(txt, res)
////        case PartLine(i) =>
////          res = SLine(i, res)
////      }
////    }
////    res
//  }
//
//  @tailrec
//  private def best2(width: Int, column: Int, docs: Docs, continuation: SimpleDoc => Unit): Unit = docs match {
//    case Nil =>
//      continuation(SEmpty)
//    case (_, EmptyDoc) :: tail =>
//      best2(width, column, tail, continuation)
//    case (i, ConsDoc(first, second)) :: tail =>
//      best2(width, column, (i, first) :: (i, second) :: tail, continuation)
//    case (i, NestDoc(j, inner)) :: tail =>
//      best2(width, column, (i + j, inner) :: tail, continuation)
//    case (i, TextDoc(text)) :: tail =>
////      SText(text, best(width, column + text.length, tail))
//      best2(width, column + text.length, tail, res => continuation(SText(text, res)))
//    case (i, LineDoc(_)) :: tail =>
////      SLine(i, best(width, i, tail))
//      best2(width, i, tail, res => continuation(SLine(i, res)))
//    case (i, UnionDoc(l, s)) :: tail =>
//      best2(width, column, (i, l) :: tail, continueWithBetter(width, column, (i,s) :: tail, continuation))
////      best(width, column, (i, l) :: tail, r1 => {
////        if (r1.fits(width - column))
////          continuation(r1)
////        else
////          best(width, column, (i, s) :: tail, continuation)
////        })
//    case (i, AlignDoc(inner)) :: tail =>
//      best2(width, column, (column, inner) :: tail, continuation)
//    case (i, ColumnDoc(f)) :: tail =>
//      best2(width, column, (i, f(column)) :: tail, continuation)
//  }
//
//  def continueWithBetter(width: Int, column: Int, tuples: List[(Int, Doc)], continuation: (SimpleDoc) => Unit)(d1: SimpleDoc): Unit = {
//    if (d1.fits(width - column))
//      continuation(d1)
//    else
//      best2(width, column, tuples, continuation)
//  }
//
//  private def better(width: Int, column: Int, d1: SimpleDoc, d2: () => SimpleDoc): SimpleDoc =
//    if (d1.fits(width - column))
//      d1
//    else
//    // d2 is computed only if needed...
//      d2()
//
//}
