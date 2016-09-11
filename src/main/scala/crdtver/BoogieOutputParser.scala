package crdtver

import crdtver.BoogieAst.{AstElementTraceInfo, Element, EndAtomicTraceInfo, TextTraceInfo}
import crdtver.InputAst.{CrdtCall, InInvariantDecl, SourcePosition, SourceRange}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer


/**
  * Created by peter on 10.09.16.
  */
class BoogieOutputParser {


  private val traceLine = "    (.*?)\\(([0-9]+),([0-9]+)\\): (.*?)".r
  private val labelLine = "(.*?)\\(([0-9]+),([0-9]+)\\): (.*?)".r


  private var errors = List[BoogieError]()

  case class BoogieError(parts: List[BoogieErrorPart], executionTrace: List[TraceLine])

  case class BoogieErrorPart(line: Int, column: Int, message: String)
  case class TraceLine(line: Int, column: Int, message: String)


  def parse(output: String):Unit = {
    var currentParts = ListBuffer[BoogieErrorPart]()
    var currentTrace = ListBuffer[TraceLine]()
    var traceStarted = false

    def finishPart(): Unit = {
      if (currentParts.nonEmpty) {
        errors = errors ++ List(BoogieError(currentParts.toList, currentTrace.toList))
      }

      currentParts = ListBuffer[BoogieErrorPart]()
      currentTrace = ListBuffer[TraceLine]()
      traceStarted = false
    }

    for (line <- output.lines) {
      line match {
        case "Execution trace:" =>
          traceStarted = true
        case traceLine(file, lineNr, column, message) =>
          currentTrace += TraceLine(lineNr.toInt, column.toInt, message)
        case labelLine(file, lineNr, column, message) =>
          if (traceStarted) {
            finishPart()
          }
          currentParts += BoogieErrorPart(lineNr.toInt, column.toInt, message)
        case _ =>
      }
    }
    finishPart()
  }

  case class VerificationError(source: Option[SourceRange], message: String) {
    override def toString: String = source match {
      case Some(range) =>
        s"[Line ${range.start.line}] $message"
      case None =>
        message
    }
  }

  def extractText(range: SourceRange, inputText: String): String = {
    val lines: Array[String] = inputText.lines.slice(range.start.line-1, range.stop.line).toArray
    if (lines.isEmpty) {
      ""
    } else {
      lines(lines.length-1) = lines(lines.length-1).substring(0, range.stop.column)
      lines(0) = lines(0).substring(range.start.column)
      lines.mkString("\n")
    }
  }

  def printErrors(sourceMap: TreeMap[Int, BoogieAst.Element], inputText: String): Unit = {
    val errorList: ListBuffer[VerificationError] = getErrors(sourceMap, inputText)


    for (err <- errorList) {
      println(err)
    }
  }

  def errorCount(): Int = {
    errors.size
  }

  def getErrors(sourceMap: TreeMap[Int, Element], inputText: String): ListBuffer[VerificationError] = {
    val errorList = ListBuffer[VerificationError]()

    for (err <- errors) {
      for (part <- err.parts) {
        val msg = part.message
        for (source <- getSource(part.line, sourceMap)) {
          source.trace match {
            case null =>
            case AstElementTraceInfo(inputSource) =>
              inputSource match {
                case inv: InInvariantDecl =>
                  val invText = extractText(inv.getSource().range, inputText)
                  errorList += VerificationError(
                    source = Some(inputSource.getSource().range),
                    message = s"$msg:\n$invText"
                  )
                case _ =>
                  errorList += VerificationError(
                    source = Some(inputSource.getSource().range),
                    message = msg
                  )
              }
            case EndAtomicTraceInfo(inputSource) =>
              inputSource match {
                case crdtCall: CrdtCall =>
                  errorList += VerificationError(
                    source = Some(inputSource.getSource().range),
                    message = s"when commiting call to ${crdtCall.call.functionName}: $msg"
                  )
                case _ =>
                  val stop: SourcePosition = inputSource.getSource().stop
                  errorList += VerificationError(
                    source = Some(SourceRange(stop.copy(column = stop.column - 1), stop)),
                    message = s"when commiting atomic block: $msg"
                  )
              }
            case TextTraceInfo(text) =>
              errorList += VerificationError(
                source = None,
                message = s"$text:\n$msg"
              )
          }
        }
      }

      for (tr <- err.executionTrace) {
        for (source <- getSource(tr.line, sourceMap)) {
          val range = source.trace match {
            case null =>
              None
            case AstElementTraceInfo(source) =>
              Some(source.getSource().range)
            case EndAtomicTraceInfo(source) =>
              Some(source.getSource().range)
            case TextTraceInfo(text) =>
              None
          }
          errorList += VerificationError(
            source = range,
            message = s"   trace: ${tr.message}"
          )
        }
      }
    }
    errorList
  }

  def getSource(lineNr: Int, sourceMap: TreeMap[Int, BoogieAst.Element]): Option[Element] = {
    val beforeLine: TreeMap[Int, Element] = sourceMap.until(lineNr+1)
    val reversed = TreeMap[Int, Element]()(implicitly[Ordering[Int]].reverse) ++ beforeLine
    for ((i,e) <- reversed) {
      if (e.trace != null) {
        return Some(e)
      }
    }
    None
  }




}
