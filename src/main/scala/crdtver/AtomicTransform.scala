package crdtver

import crdtver.InputAst._

import scala.collection.mutable.ListBuffer


/**
  * puts all calls and queries, which are not inside a transaction into a singleton transaction
  */
object AtomicTransform {

  def transformProg(prog: InProgram): InProgram = {

    val queries = prog.queries.map(_.name.name)

    prog.copy(
      procedures = prog.procedures.map(transformProcedure(_, queries))
    )
  }



  def transformProcedure(proc: InProcedure, queries: List[String]): InProcedure = {
    val newLocals = ListBuffer[InVariable]()
    var counter = 0
    def newLocal(typ: InTypeExpr): String = {
      counter += 1
      val name = s"__query_$counter"
      newLocals += InVariable(NoSource(), Identifier(NoSource(), name), typ)
      name
    }

    def transformStatement(s: InStatement): InStatement = s match {
      case b @ BlockStmt(source, stmts) =>
        makeBlock(
          source,
          stmts.map(transformStatement)
        )
      case atomic: Atomic =>
        atomic
      case l: LocalVar =>
        l
      case IfStmt(source, cond, thenStmt, elseStmt) =>
        val (condT, stmts) = transformExpr(cond)
        makeBlock(
          source,
          stmts
          :+ IfStmt(source, condT, transformStatement(thenStmt), transformStatement(elseStmt))
        )
      case MatchStmt(source, expr, cases) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts
          :+ MatchStmt(
            source = source,
            expr = exprT,
            cases = cases.map(c => c.copy(statement = transformStatement(c.statement)))
          )
        )
      case CrdtCall(source, call) =>
        Atomic(source,
            CrdtCall(source, call)
        )
      case Assignment(source, varname, expr) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts :+ Assignment(source, varname, exprT)
        )
      case n @ NewIdStmt(source, varname, typename) =>
        n
      case ReturnStmt(source, expr) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts :+ ReturnStmt(source, exprT)
        )
    }

    def transformExpr(e: InExpr): (InExpr, List[InStatement]) = e match {
      case v: VarUse =>
        (v, List())
      case call @ FunctionCall(src, typ, functionName, args) =>
        val transformed = args.map(transformExpr)
        val stmts = transformed.flatMap(_._2)
        val args2 = transformed.map(_._1)

        if (queries.contains(functionName.name)) {
          val localName = newLocal(typ)
          val queryBlock = Atomic(src,
            Assignment(src, Identifier(src, localName), call.copy(args = args2))
          )
          (VarUse(src, typ, localName), stmts :+ queryBlock)
        } else {
          (call.copy(args = args2), stmts)
        }
      case appB @ ApplyBuiltin(source, typ, function, args) =>
        val transformed = args.map(transformExpr)
        val stmts = transformed.flatMap(_._2)
        (appB.copy(args = transformed.map(_._1)), stmts)
      case q: QuantifierExpr =>
        // TODO typechecker ensures that quantifiers contain no queries outside of atomic blocks
        (q, List())

    }



    val newBody = transformStatement(proc.body)
    proc.copy(
      locals = proc.locals ++ newLocals,
      body = newBody
    )
  }

}
