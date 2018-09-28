package crdtver.symbolic

class SymbolicContext {
  def addConstraint(constraint: SVal[SortBoolean]): Unit = ???

  def makeVariable[T <: SymbolicSort](name: String)(implicit sort: T): SymbolicVariable[T] =
  // TODO make unique name
    SymbolicVariable(name, sort)


  /**
    * Executes some code in a new context.
    *
    * When executing it on Z3 this means
    * pushing a new frame when entering the code block
    * and popping a frame afterwards
    **/
  def inContext(branch: () => Unit): Unit = ???

}


