package crdtver

import java.io.{ByteArrayInputStream, File, FileReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import crdtver.parser.BoogieModelParser.{ExprContext, ModelContext, VariableValueContext}
import resource.managed
import crdtver.parser.{BoogieModelLexer, BoogieModelParser}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import scala.collection.immutable.{SortedMap, SortedSet}

import org.apache.commons.lang3.StringEscapeUtils

class ModelInterpreter {


  def load(file: File): Unit = load(file.toPath)

  def load(fileName: String): Unit = {
    load(new File(fileName))
  }


  def load(input: Path): Unit = {
    for (reader <- managed(Files.newBufferedReader(input))) {
      val inStream = new ANTLRInputStream(reader)
      val lex = new BoogieModelLexer(inStream)
      val tokenStream = new CommonTokenStream(lex)
      val parser = new BoogieModelParser(tokenStream)

      val model = parser.model()

      //      println(s"loaded model: ${model.toStringTree(parser)}")

      visualizeModel(model)

    }
  }


  def visualizeModel(model: ModelContext) = {
    val htmlOut = new StringBuilder
    htmlOut append
      s"""
         |<!DOCTYPE html>
         |<html lang="en">
         |	<head>
         |		<meta charset="utf-8">
         |		<meta http-equiv="X-UA-Compatible" content="IE=edge">
         |    <meta name="viewport" content="width=device-width, initial-scale=1">
         |		<title>Model output</title>
         |		<link rel="stylesheet" href="style.css">
         |		<script src="script.js"></script>
         |		<!-- Latest compiled and minified CSS -->
         |		<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
         |           integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
         |
         |		<!-- Optional theme -->
         |		<!-- <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
         |           integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">-->
         |
         |    <style type="text/css">
         |    .highlight {
         |      background-color: #cdf;
         |    }
         |    </style>
         |</head>
         |<body>
         |<div class="container">
       """.stripMargin


    var variableState = SortedMap[String, ExprContext]()

    for (state <- model.state().asScala) {
      htmlOut append
        s"""
           |<h3>State ${StringEscapeUtils.escapeHtml4(state.startToken.getText)}</h3>
         """.stripMargin

      val changedVars: Set[String] =
        (for (v <- state.variableValues.asScala) yield {
          val varName = v.varName.getText
          variableState += (varName -> v.value)
          varName
        }).toSet


      htmlOut append
        """
          |<table>
          |<tr><th>Variable</th><th>Value</th></tr>
        """.stripMargin


      for ((varName, varValue) <- variableState) {
        htmlOut append
          s"""
             |<tr ${if (changedVars.contains(varName)) """class="highlight"  """ else ""}>
             |   <td>$varName</td>
             |   <td>${varValue.getText}</td>
             |</tr>
           """.stripMargin
      }

      htmlOut append """</table>"""

      val svg: String = printEventGraph(model, variableState)

      htmlOut append s"""<div>$svg</div>"""
    }


    htmlOut append
      s"""
         |</div>
         |<div style="height:500px"></div>
         |		<!-- Latest compiled and minified JavaScript -->
         |		<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
         |           integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>
         |</body></html>""".stripMargin


    Files.write(Paths.get("model/model.html"), htmlOut.toString().getBytes(StandardCharsets.UTF_8))
  }

  case class CallId(id: String) extends Comparable[CallId] {
    override def compareTo(t: CallId): Int = id.compareTo(t.id)

    override def toString: String = s"c_$id"
  }

  sealed abstract class Operation()

  case class OperationAtom(name: String) extends Operation {
    override def toString: String = name
  }

  case class OperationApply(ops: List[Operation]) extends Operation {
    override def toString: String = ops match {
      case f :: args =>
        s"$f(${args.mkString(", ")})"
    }
  }

  var stateCounter: Int = 0

  def genDotFile(operations: Map[CallId, Operation], visibleCalls: Set[CallId], happensBefore: Set[(CallId, CallId)]): String = {
    val dot = new StringBuilder
    def appendLine(s: String): Unit = {
      dot append s
      dot append "\n"
    }

    def append(s: String): Unit = {
      dot append s
    }

    appendLine("digraph g {")
    appendLine("page = \"8.2677165,11.692913\" ;")
//    appendLine("overlap=prism; overlap_scaling=0.01; ratio=0.7;")

    // draw nodes
    for ((callId, op) <- operations) {
      val style =
        if (visibleCalls contains callId)
          "style=\"rounded\""
        else
          "style=\"rounded\",color=\"#aaaaaa\",fontcolor=\"#aaaaaa\""

      appendLine(s"""    $callId [shape="box", $style, label="$callId\n$op"];""")
//      appendLine(s"""    ${callId}_text [shape=plaintext,label="$op"];\n""")
//      appendLine(s"""    {rank=same; $callId, ${callId}_text}\n""")
    }

    // draw edges
    for ((c1,c2) <- happensBefore) {
      appendLine(s"""    $c1 -> $c2;""")
    }


    appendLine("}")
    Files.write(Paths.get(s"model/state$stateCounter.dot"), dot.toString().getBytes(StandardCharsets.UTF_8))
    stateCounter += 1

    import sys.process._
    val input = new ByteArrayInputStream(dot.toString().getBytes())
    val out = ("tred" #< input) #| "dot -Tsvg"
    val lines = out.lineStream_!.drop(3).mkString("\n")

    lines
  }

  def printEventGraph(model: ModelContext, variableState: SortedMap[String, ExprContext]): String = {
    val state_callOps = variableState("state_callOps").getText
    val state_visibleCalls = variableState("state_visibleCalls").getText
    val state_happensBefore = variableState("state_happensBefore").getText
    val state_sameTransaction = variableState("state_sameTransaction").getText
    val state_currentTransaction = variableState("state_currentTransaction").getText

    val operations: Map[CallId, Operation] = extractOperations(model, state_callOps)

    val visibleCalls: Set[CallId] = extractVisibleCalls(model, state_visibleCalls)

    val happensBefore: Set[(CallId, CallId)] = extractHappensBefore(model, state_happensBefore)

    val dot = genDotFile(operations, visibleCalls, happensBefore)



//    s"""operations = $operations
//        |<br /><br />
//        |vis = $visibleCalls
//        |<br /><br />
//        |hb = $happensBefore
//        |""".stripMargin
    dot
  }


  def extractOperations(model: ModelContext, state_callOps: String): Map[CallId, Operation] = {
    val ops = getVariableValue(model, "Select_[callId]operation").get

    var result = SortedMap[CallId, Operation]()

    for (te <- ops.tableEntry().asScala) {
      te.keys.toList match {
        case k1 :: callId :: Nil =>
          if (k1.getText == state_callOps && te.value.getText != "(noop)") {
            result += (makeCalId(callId) -> makeOperation(te.value))
          }

        case _ =>
      }
    }


    result
  }

  def extractVisibleCalls(model: ModelContext, state_visibleCalls: String): Set[CallId] = {
    val vis = getVariableValue(model, "Select_[callId]$bool").get

    var result = SortedSet[CallId]()
    for (te <- vis.tableEntry().asScala) {
      te.keys.toList match {
        case k1 :: callId :: Nil =>
          if (k1.getText == state_visibleCalls && te.value.getText == "true") {
            result += makeCalId(callId)
          }

        case _ =>
      }
    }
    result
  }

  def extractHappensBefore(model: ModelContext, state_happensBefore: String): Set[(CallId, CallId)] = {
    val vis = getVariableValue(model, "Select_[callId,callId]$bool").get

    var result = SortedSet[(CallId, CallId)]()
    for (te <- vis.tableEntry().asScala) {
      te.keys.toList match {
        case k1 :: callId1 :: callId2 :: Nil =>
          if (k1.getText == state_happensBefore && te.value.getText == "true") {
            result += ((makeCalId(callId1), makeCalId(callId2)))
          }

        case _ =>
      }
    }
    result
  }


  def makeCalId(callId: ExprContext): CallId = {
    CallId(callId.sExprValues(1).getText)
  }

  def makeOperation(value: ExprContext): Operation = {
    if (value.varName != null) {
      OperationAtom(value.getText)
    } else {
      OperationApply(value.sExprValues.toList.map(makeOperation))
    }
  }

  def getVariableValue(model: ModelContext, s: String): Option[ExprContext] = {
    for (a <- model.variableValues.asScala) {
      if (a.varName.getText == s) {
        return Some(a.value)
      }
    }
    None
  }


}
