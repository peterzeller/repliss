package crdtver.language

import crdtver.language.InputAst.BuiltInFunc
import crdtver.language.TypedAst.DatabaseCall
import crdtver.language.crdts.CrdtInstance
import crdtver.utils.PrettyPrintDoc.Doc
;

object TypedAstPrinter {

  import crdtver.utils.PrettyPrintDoc._

  def printCrdt(programCrdt: CrdtInstance): Doc = ""

  def printDeclaration(declaration: TypedAst.InDeclaration): Doc = declaration match {
    case TypedAst.InProcedure(source, name, params, locals, returnType, body) =>
      "def" <+> name.toString + "(" <> sep(", ", params.map(print)) <> ")" <>
        (returnType match {
          case Some(value) => ": " <> printType(value)
          case None => ""
        }) <+> "{" <>
        nested(2,
          line <> sep(line, locals.map(l => "var " <> print(l))) </>
            print(body)
        ) </>
        "}"
    case TypedAst.InTypeDecl(source, isIdType, name, dataTypeCases) =>
      if (isIdType) {
        "idtype"
      } else {
        "type"
      } <+> name.toString <+>
        (if (dataTypeCases.isEmpty) {
          ""
        } else {
          " = " <>
            nested(2,
              line <> "  " <> sep(line <> "| ", dataTypeCases.map(print))
            )
        })
    case TypedAst.InOperationDecl(source, name, params) =>
      "operation" <+> name.toString <> "(" <> sep(", ", params.map(print)) <> ")"
    case TypedAst.InQueryDecl(source, name, params, returnType, implementation, ensures, annotations) =>
      "query" <+> name.toString <> "(" <> sep(", ", params.map(print)) <> "): " <> printType(returnType)
    case TypedAst.InAxiomDecl(source, expr) =>
      "axiom" <+> printExpr(expr)
    case TypedAst.InInvariantDecl(source, expr) =>
      "invariant" <+> printExpr(expr)
    case TypedAst.InCrdtDecl(source, keyDecl) =>
      "crdt" <+> print(keyDecl)
  }

  def printCrdtType(crdtType: TypedAst.InCrdtType): Doc = crdtType match {
    case TypedAst.InCrdt(source, name, typ) =>
      name.toString <> "<" <> sep(", ", typ.map(print)) <> ">"
    case TypedAst.InStructCrdt(source, keyDecl) =>
      "{" <> nested(2, line <> sep("," <> line, keyDecl.map(k => print(k) <> line))) </> "}"
  }

  def printExpr(expr: TypedAst.InExpr): Doc = expr match {
    case TypedAst.VarUse(source, typ, name) =>
      name.toString
    case TypedAst.BoolConst(source, typ, value) =>
      value.toString
    case TypedAst.IntConst(source, typ, value) =>
      value.toString()
    case expr: TypedAst.CallExpr =>
      expr match {
        case TypedAst.FunctionCall(source, typ, functionName, args, kind) =>
          group(functionName.toString <> "(" <> nested(2, line <> sep(", ", args.map(e => printExpr(e) <> line)) <> ")"))
        case DatabaseCall(_, t, i, operation) =>
          "call " <+> printExpr(operation)
        case TypedAst.ApplyBuiltin(source, typ, function, args) =>
          def binary(op: String): Doc = 
            group("(" <> nested(2, printExpr(args(0))) </> op <+> nested(4, printExpr(args(1))) <> ")")
          
          function match {
            case BuiltInFunc.BF_isVisible() =>
              printExpr(args(0)) <> ".isVisible"
            case BuiltInFunc.BF_happensBefore(on) =>
              binary("happensBefore")
            case BuiltInFunc.BF_sameTransaction() =>
              binary("inSameTransactionAs")
            case BuiltInFunc.BF_less() =>
              binary("<")
            case BuiltInFunc.BF_lessEq() =>
              binary("<=")
            case BuiltInFunc.BF_greater() =>
              binary(">")
            case BuiltInFunc.BF_greaterEq() =>
              binary(">=")
            case BuiltInFunc.BF_equals() =>
              binary("==")
            case BuiltInFunc.BF_notEquals() =>
              binary("!=")
            case BuiltInFunc.BF_and() =>
              binary("&&")
            case BuiltInFunc.BF_or() =>
              binary("||")
            case BuiltInFunc.BF_implies() =>
              binary("==>")
            case BuiltInFunc.BF_not() =>
              "!" <> printExpr(args(0))
            case BuiltInFunc.BF_plus() =>
              binary("+")
            case BuiltInFunc.BF_minus() =>
              binary("-")
            case BuiltInFunc.BF_mult() =>
              binary("*")
            case BuiltInFunc.BF_div() =>
              binary("/")
            case BuiltInFunc.BF_mod() =>
              binary("%")
            case BuiltInFunc.BF_getOperation() =>
              printExpr(args(0)) <> ".op"
            case BuiltInFunc.BF_getInfo() =>
              printExpr(args(0)) <> ".info"
            case BuiltInFunc.BF_getResult() =>
              printExpr(args(0)) <> ".result"
            case BuiltInFunc.BF_getOrigin() =>
              printExpr(args(0)) <> ".origin"
            case BuiltInFunc.BF_getTransaction() =>
              printExpr(args(0)) <> ".tx"
            case BuiltInFunc.BF_inCurrentInvoc() =>
              printExpr(args(0)) <> ".inCurrentInvoc"
          }
      }
    case TypedAst.QuantifierExpr(source, typ, quantifier, vars, expr) =>
      val q: Doc = quantifier match {
        case InputAst.Forall() => "forall"
        case InputAst.Exists() => "exists"
      }
      group(q <+> sep(", ", vars.map(print)) <+> "::" <> nested(4, line <> print(expr)))
    case TypedAst.InAllValidSnapshots(expr) =>
      group("(in all valid snapshots :: " <> nested(4, line <> print(expr)) <> ")")
  }

  def printStatement(statement: TypedAst.InStatement): Doc = statement match {
    case TypedAst.BlockStmt(source, stmts) =>
      "{" <>
        nested(2, line <> sep(line, stmts.map(printStatement))) </>
        "}"
    case TypedAst.Atomic(source, body) =>
      "atomic" <+> printStatement(body)
    case TypedAst.LocalVar(source, variable) =>
      "var" <+> print(variable)
    case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      "if (" <> printExpr(cond) <> ") " <> printStatement(thenStmt) <+> "else" <+> printStatement(elseStmt)
    case TypedAst.MatchStmt(source, expr, cases) =>
      printExpr(expr) <+> "match {" </>
        nested(2, sep(line, cases.map(c => print(c) <> line)))
      "}"
    case TypedAst.CrdtCall(source, None, instance, call) =>
      "call <" <> instance.toString <> ">" <> print(call)
    case TypedAst.CrdtCall(source, Some(result), instance, call) =>
      "call" <+> result.toString <+> "= <" <> instance.toString <> ">" <> print(call)
    case TypedAst.Assignment(source, varname, expr) =>
      varname.toString <+> "=" <+> printExpr(expr)
    case TypedAst.NewIdStmt(source, varname, typename) =>
      varname.toString <+> " = new " <> printType(typename)
    case TypedAst.ReturnStmt(source, expr, assertions) =>
      "return" <+> printExpr(expr) <>
        nested(2, sep("", assertions.map(a => line <> "assert" <+> print(a))))
    case TypedAst.AssertStmt(source, expr) =>
      "assert" <+> printExpr(expr)
  }

  def printType(typ: TypedAst.InTypeExpr): Doc = typ match {
    case TypedAst.AnyType() => "any"
    case TypedAst.BoolType() => "Bool"
    case TypedAst.IntType() => "Int"
    case TypedAst.CallIdType() => "CallId"
    case TypedAst.InvocationIdType() => "InvocationId"
    case TypedAst.TransactionIdType() => "TransactionId"
    case TypedAst.InvocationInfoType() => "InvocationInfo"
    case TypedAst.InvocationResultType() => "InvocationResult"
    case TypedAst.SomeOperationType() => "SomeOperation"
    case TypedAst.OperationType(name, resultType) => "Operation<" <> name.toString <> ", " <> printType(resultType) <> ">"
    case TypedAst.FunctionType(argTypes, returnType, functionKind) =>
      "(" <> sep(", ", argTypes.map(printType)) <> ") => " <> printType(returnType)
    case TypedAst.SimpleType(name) =>
      name.toString
    case TypedAst.IdType(name) =>
      name.toString
    case TypedAst.CrdtTypeDefinitionType(c) => s"CRDT#${c.name}"
    case TypedAst.NestedOperationType(operations) => s"NestedOperations(${operations.map(_.name).mkString(", ")})"
    case TypedAst.DependentReturnType(operations) => s"DependentReturnType(${operations.map(_.name).mkString(", ")})"
    case TypedAst.TypeUnit() => "Unit"
  }

  def print(elem: TypedAst.AstElem): Doc = elem match {
    case TypedAst.InProgram(name, source, procedures, types, axioms, invariants, programCrdt) =>
      sep(line, types.map(print)) </>
        sep(line, axioms.map(print)) </>
        sep(line, procedures.map(print)) </>
        sep(line, invariants.map(print)) </>
        printCrdt(programCrdt)
    case declaration: TypedAst.InDeclaration =>
      printDeclaration(declaration)
    case TypedAst.DataTypeCase(source, name, params, returnTyp) =>
      name.toString <> "(" <> sep(", ", params.map(print)) <> ")"
    case TypedAst.InKeyDecl(source, name, crdttype) =>
      name.toString <> ":" <+> print(crdttype)
    case crdtType: TypedAst.InCrdtType =>
      printCrdtType(crdtType)
    case TypedAst.InVariable(source, name, typ) =>
      name.toString <> ":" <+> print(typ)
    case expr: TypedAst.InExpr =>
      printExpr(expr)
    case statement: TypedAst.InStatement =>
      printStatement(statement)
    case TypedAst.MatchCase(source, pattern, statement) =>
      "case" <+> print(pattern) <+> "=>" </> printStatement(statement)
    case typ: TypedAst.InTypeExpr =>
      printType(typ)
  }


}
