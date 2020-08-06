package crdtver.language

import TypedAst._
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.FunctionKind.{FunctionKindCrdtQuery, FunctionKindDatatypeConstructor}

import scala.collection.mutable.ListBuffer


/**
  * Transforms an input program and
  * puts all calls and queries, which are not inside a transaction into a singleton transaction
  */
object AtomicTransform {



  def transformProg(prog: InProgram): InProgram = {

    prog.copy(
      procedures = prog.procedures.map(transformProcedure(_, prog)),
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

  case class Context(prog: InProgram, inAtomic: Boolean = false)

  def transformProcedure(proc: InProcedure, prog: InProgram): InProcedure = {
    val newLocals = ListBuffer[InVariable]()
    var counter = 0
    def newLocal(vname: String, typ: InTypeExpr): String = {
      counter += 1
      val name = s"__${vname}_$counter"
      newLocals += InVariable(NoSource(), Identifier(NoSource(), name), typ)
      name
    }

    def transformStatement(s: InStatement)(implicit ctxt: Context): InStatement = s match {
      case b @ BlockStmt(source, stmts) =>
        makeBlock(
          source,
          stmts.map(transformStatement)
        )
      case atomic @ Atomic(source, body) =>
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
      case CrdtCall(source, call) =>
        val transformed = call.args.map(transformExpr)
        val stmts = transformed.flatMap(_._2)
        val args2 = transformed.map(_._1)
        makeBlock(source,
          stmts
          :+ (if (ctxt.inAtomic) {
            CrdtCall(source, call.copy(args = args2))
          } else {
            Atomic(source,
              CrdtCall(source, call.copy(args = args2))
            )
          })
        )
      case Assignment(source, varname, expr) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts :+ Assignment(source, varname, exprT)
        )
      case n @ NewIdStmt(source, varname, typename) =>
        n
      case ReturnStmt(source, expr, assertions) =>
        val (exprT, stmts) = transformExpr(expr)
        makeBlock(source,
          stmts :+ ReturnStmt(source, exprT, assertions)
        )
      case a: AssertStmt =>
        a
    }

    def transformExpr(e: InExpr)(implicit ctxt: Context): (InExpr, List[InStatement]) = e match {
      case v: VarUse =>
        (v, List())
      case b: BoolConst =>
        (b, List())
      case i: IntConst =>
        (i, List())
      case call @ FunctionCall(src, typ, functionName, typeArgs, args, kind) =>
        val transformed = args.map(transformExpr)
        val stmts = transformed.flatMap(_._2)
        val args2 = transformed.map(_._1)
        (call.copy(args = args2), stmts)

      case call @ CrdtQuery(src, typ, qryOp) =>
        val transformed = transformExpr(qryOp)
        val stmts = transformed._2
        val qryOp2 = transformed._1.asInstanceOf[FunctionCall]

        val flat = ctxt.prog.programCrdt.toFlatQuery[InExpr](qryOp2).get
        val qryName = flat.name

        val localName = newLocal(s"query_${qryName}_res", typ)
        var queryStatements: InStatement = makeBlock(src, List(
          // res =
          Assignment(src, Identifier(src, localName),
            FunctionCall(src, typ, Identifier(src, qryName), List(), flat.args, FunctionKindCrdtQuery())),
          CrdtCall(src, FunctionCall(src, CallInfoType(), Identifier(src, "Qry"), List(), List(qryOp2), FunctionKindDatatypeConstructor()))
        ))
        if (!ctxt.inAtomic) {
          // wrap in atomic block:
          queryStatements = Atomic(src, queryStatements)
        }
        (VarUse(src, typ, localName), stmts :+ queryStatements)

      case appB @ ApplyBuiltin(source, typ, function, args) =>
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



    val newBody = transformStatement(proc.body)(Context(prog))
    proc.copy(
      locals = proc.locals ++ newLocals,
      body = newBody
    )
  }

}
