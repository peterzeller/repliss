package crdtver.language

import TypedAst.{InExpr, _}
import crdtver.language.InputAst.{Identifier, NoSource}

import scala.collection.mutable.ListBuffer


/**
  * Transforms an input program and
  * puts all calls and queries, which are not inside a transaction into a singleton transaction
  */
object AtomicTransform {


  def transformProg(prog: InProgram): InProgram = {

    val queries: List[String] = prog.programCrdt.queryDefinitions.map(_.name.name)

    prog.copy(
      procedures = prog.procedures.map(transformProcedure(_, queries)),
    )
  }

  def queryOperations(queries: List[InQueryDecl]): List[InOperationDecl] = queries.map(makeQueryOperation)

  def makeQueryOperation(query: InQueryDecl): InOperationDecl = {
    InOperationDecl(
      source = query.source,
      name = Identifier(query.source, "queryop_" + query.name),
      params = query.params :+ InVariable(query.source, Identifier(query.source, "result"), query.returnType)
    )
  }

  case class Context(inAtomic: Boolean = false)

  def transformProcedure(proc: InProcedure, queries: List[String]): InProcedure = {
    val newLocals = ListBuffer[InVariable]()
    var counter = 0

    def newLocal(vname: String, typ: InTypeExpr): String = {
      counter += 1
      val name = s"__${vname}_$counter"
      newLocals += InVariable(NoSource(), Identifier(NoSource(), name), typ)
      name
    }

    def transformStatement(s: InStatement)(implicit ctxt: Context): InStatement = s match {
      case b@BlockStmt(source, stmts) =>
        makeBlock(
          source,
          stmts.map(transformStatement)
        )
      case atomic@Atomic(source, body) =>
        Atomic(source, transformStatement(body)(ctxt.copy(inAtomic = true)))
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
      case CrdtCall(source, resVar, instance, operation) =>
        val (newOperation, stmts) = transformFunctionCall(operation)
        val newCall = CrdtCall(source, resVar, instance, newOperation)
        val newCall2 =
          if (ctxt.inAtomic) {
            newCall
          } else {
            Atomic(source, newCall)
          }
        makeBlock(source, stmts :+ newCall2)
      case Assignment(source, varname, expr) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts :+ Assignment(source, varname, exprT)
        )
      case n@NewIdStmt(source, varname, typename) =>
        n
      case ReturnStmt(source, expr, assertions) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts :+ ReturnStmt(source, exprT, assertions)
        )
      case a: AssertStmt =>
        a
    }

    def transformFunctionCall(call: FunctionCall)(implicit ctxt: Context): (FunctionCall, List[InStatement]) = {
      val transformed = call.args.map(transformExpr)
      val stmts = transformed.flatMap(_._2)
      val args2 = transformed.map(_._1)
      (call.copy(args = args2), stmts)
    }

    def transformExpr(e: InExpr)(implicit ctxt: Context): (InExpr, List[InStatement]) = e match {
      case v: VarUse =>
        (v, List())
      case b: BoolConst =>
        (b, List())
      case i: IntConst =>
        (i, List())
      case call: FunctionCall =>
        transformFunctionCall(call)
      case call@DatabaseCall(src, typ, i, op) =>
        val (newOp1, stmts) = transformExpr(op)
        val newOp = newOp1.asInstanceOf[FunctionCall]
        val localName = newLocal(s"query_${op.functionName.name}_res", typ)
        var queryStatement: InStatement =
          CrdtCall(src, Some(localName), i, newOp)
        if (!ctxt.inAtomic) {
          // wrap in atomic block:
          queryStatement = Atomic(src, queryStatement)
        }
        (VarUse(src, typ, localName), stmts :+ queryStatement)
      case appB@ApplyBuiltin(source, typ, function, args) =>
        val transformed = args.map(transformExpr)
        val stmts = transformed.flatMap(_._2)
        (appB.copy(args = transformed.map(_._1)), stmts)
      case q: QuantifierExpr =>
        // TODO typechecker ensures that quantifiers contain no queries outside of atomic blocks
        (q, List())
      case q: InAllValidSnapshots =>
        // can only be used in invariants
        (q, List())

    }


    val newBody = transformStatement(proc.body)(Context())
    proc.copy(
      locals = proc.locals ++ newLocals,
      body = newBody
    )
  }


}
