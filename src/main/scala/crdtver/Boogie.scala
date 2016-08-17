package crdtver

object Boogie {

  // adapted from the AST description at https://github.com/boogie-org/boogie/blob/master/Source/Core/Absy.cs


  sealed abstract class Absy

  sealed abstract class AbsyContracts extends Absy

  case class Program(topLevelDeclarations: List[Declaration])
    extends Absy


  sealed abstract class Declaration(attributes: QKeyValue)
    extends Absy

  sealed abstract class DeclarationContracts(attributes: QKeyValue)
    extends Declaration(attributes)

  case class Axiom(attributes: QKeyValue, expression: Expr)
    extends Declaration(attributes)


  sealed abstract class NameDeclaration(attributes: QKeyValue, name: String)
    extends Declaration(attributes)


  case class TypeCtorDecl(attributes: QKeyValue, name: String, arity: Int)
    extends NameDeclaration(attributes, name)

  case class TypeSynonymDecl(attributes: QKeyValue, name: String, typeParameters: List[TypeVariable])
    extends NameDeclaration(attributes, name)


  sealed abstract class Variable(attributes: QKeyValue, typedIdent: TypedIdent)
    extends NameDeclaration(attributes, typedIdent.name)


  case class Constant(attributes: QKeyValue, typedIdent: TypedIdent, isUnique: Boolean)
    extends Variable(attributes, typedIdent)

  case class GlobalVariable(attributes: QKeyValue, typedIdent: TypedIdent)
    extends Variable(attributes, typedIdent)

  case class Formal(attributes: QKeyValue, typedIdent: TypedIdent, isIncoming: Boolean)
    extends Variable(attributes, typedIdent)


  case class LocalVariable(attributes: QKeyValue, typedIdent: TypedIdent)
    extends Variable(attributes, typedIdent)

  case class BoundVariable(attributes: QKeyValue, typedIdent: TypedIdent)
    extends Variable(attributes, typedIdent)


  sealed abstract class DeclWithFormals(attributes: QKeyValue,
                                        name: String,
                                        typeParameters: List[TypeVariable],
                                        inParams: List[Variable],
                                        outParams: List[Variable])
    extends NameDeclaration(attributes, name)


  sealed abstract class AbsyFunction(attributes: QKeyValue,
                                     name: String,
                                     typeParameters: List[TypeVariable],
                                     inParams: List[Variable],
                                     outParams: List[Variable],
                                     comment: String,
                                     body: Expr,
                                     definitionAxiom: Axiom,
                                     otherDefinitionAxioms: List[Axiom]
                                    )
    extends DeclWithFormals(attributes, name, typeParameters, inParams, outParams)

  case class DatatypeConstructor(selectors: List[DatatypeSelector], membership: DatatypeMembership)
    extends AbsyFunction

  case class DatatypeSelector(constructor: AbsyFunction, index: Int)
    extends AbsyFunction

  case class DatatypeMembership(constructor: AbsyFunction)
    extends AbsyFunction


  // TODO
  sealed abstract class QKeyValue

  sealed abstract class Expr

  sealed abstract class TypeVariable

  sealed abstract class TypedIdent(val name: String)



}
