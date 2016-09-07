package crdtver

import crdtver.parser.BoogieModelParser.ExprContext
import crdtver.parser.LangParser.ProgramContext

sealed abstract class Type {

}



class Typer {

  val types = scala.collection.mutable.Map[ExprContext, Type]

  case class Context(program: ProgramContext, varTypes: Map[String, Type])

  def checkProgram(program: ProgramContext): Unit = {

  }


}
