package crdtver

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * See http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
  */
object PrettyPrintDoc {


  sealed abstract class Doc {


    def seqParts(): List[Doc] = this match {
      case SeqDoc(parts) => parts.flatMap(_.seqParts())
      case _ => List(this)
    }

    def <>(doc: Doc): Doc = SeqDoc(this.seqParts ++ doc.seqParts)


    def :<|>(other: () => Doc): Doc = Alternative(this, other)

    def flatten(): Doc = this match {
      case SeqDoc(parts) => SeqDoc(parts.map(_.flatten()))
      case NilDoc() => this
      case TextDoc(text) => this
      case NewlineDoc() => this
      case NestedDoc(nesting, doc) => NestedDoc(nesting, doc.flatten())
      case Alternative(first, other) => first.flatten()
    }

    private def best(w: Int, k: Int) = be(w, k, List((0, this)))

    private def be(w: Int, k: Int, l: List[(Int, Doc)]): LayoutDoc = l match {
      case Nil => LayoutNil()
      case (i, x) :: z => x match {
        case NilDoc() =>
          be(w, k, z)
        case SeqDoc(parts) =>
          be(w, k, parts.map(p => (i, p)) ++ z)
        case NestedDoc(nesting, doc) =>
          be(w, k, (i + nesting, doc) :: z)
        case TextDoc(text) =>
          LayoutText(text, be(w, k + text.length, z))
        case NewlineDoc() =>
          LayoutLine(i, be(w, i, z))
        case Alternative(first, other) =>
          val layoutFirst = be(w, k, (i,first)::z)
          if (layoutFirst.fits(w-k)) {
            layoutFirst
          } else {
            be(w, k, (i,other())::z)
          }
      }
    }

    def pretty(w: Int): LayoutDoc = best(w, 0)

    def prettyStr(w: Int): String = pretty(w).layout()


    // Utility functions

    def <+>(other: Doc) = this <> text(" ") <> other

    def </>(other: Doc) = this <> line <> other
  }

  implicit def text(str: String): Doc = TextDoc(str)

  implicit def list(docs: List[Doc]): SeqDoc = SeqDoc(docs.flatMap(_.seqParts()))

  def sep(separator: Doc, parts: List[Doc]) = {
    if (parts.isEmpty) {
      NilDoc()
    } else {
      parts.reduce((l,r) => l <> separator <> r)
    }
  }

  def line = NewlineDoc()

  def group(x: Doc) = x.flatten() :<|> (() => x)

  def nested(i: Int, doc: Doc): Doc = NestedDoc(i, doc)

  case class SeqDoc(parts: List[Doc]) extends Doc

  case class NilDoc() extends Doc

  case class TextDoc(text: String) extends Doc

  case class NewlineDoc() extends Doc

  case class NestedDoc(nesting: Int, doc: Doc) extends Doc

  case class Alternative(first: Doc, other: () => Doc) extends Doc


  sealed abstract class LayoutDoc() {

    @tailrec
    final def fits(w: Int): Boolean = {
      if (w < 0) {
        return false
      }
      this match {
        case LayoutNil() =>
          true
        case LayoutText(text, rest) =>
          rest.fits(w-text.length)
        case LayoutLine(indent, rest) =>
          true
      }
    }

    def layout(): String = {
      var doc = this
      val res = new StringBuilder
      while (true) {
        doc match {
          case LayoutNil() =>
            return res.toString()
          case LayoutText(text, rest) =>
            res.append(text)
            doc = rest
          case LayoutLine(indent, rest) =>
            res.append("\n")
            res.append(" " * indent)
            doc = rest
        }
      }
      return res.toString()
    }

  }

  case class LayoutNil() extends LayoutDoc

  case class LayoutText(text: String, rest: LayoutDoc) extends LayoutDoc

  case class LayoutLine(indent: Int, rest: LayoutDoc) extends LayoutDoc

}
