package crdtver.symbolic


abstract class SmtTranslation {


  type TBoolExpr

  type TExpr

  var datatypeImpl: SortDatatype => SortDatatypeImpl = _

  def translateBool(constraint: SVal[SortBoolean]): this.TBoolExpr

  def translateExpr[T <: SymbolicSort](expr: SVal[T]): TExpr

  def parseExpr[T <: SymbolicSort](expr: TExpr)(implicit t: T): SVal[T]

  def mkSolver(): SmtSolver


  abstract class SmtSolver {

    def push(): Unit

    def pop(): Unit

    def add(translated: TBoolExpr): Unit

    def check(): CheckRes

    sealed abstract class CheckRes()

    abstract class Satisfiable() extends CheckRes() {
      def getModel: Model
    }

    case class Unknown() extends CheckRes()

    case class Unsatisfiable() extends CheckRes()

    trait Model {
      def eval(expr: TExpr, bool: Boolean): TExpr
    }

  }

}