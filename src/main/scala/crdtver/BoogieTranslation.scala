package crdtver

import crdtver.parser.LangParser._
import crdtver.BoogieAst._

class BoogieTranslation {


  def transformProgram(programContext: ProgramContext): Program = {

    var types: Map[String, TypeDecl] = Map()
    types += ("callId" -> TypeDecl("callId"))


    for (decl: DeclarationContext <- programContext.declaration();
         typeDecl: TypedeclContext <- Option(decl.typedecl())) {
      val name: String = typeDecl.name.getText
      types += (typeDecl.name.getText -> TypeDecl(name))
    }


    for (decl: DeclarationContext <- programContext.declaration();
         procedure: ProcedureContext <- Option(decl.procedure())) {

    }



  }


  def example(): Unit = {



  }


}
