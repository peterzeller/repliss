package crdtver.testing

sealed trait LogicEvaluatorConfig {
  def needsLogicEvaluator: Boolean
}
case class UseSimpleEvaluator() extends  LogicEvaluatorConfig {
  def needsLogicEvaluator: Boolean = false
}
case class UseBoth() extends  LogicEvaluatorConfig {
  def needsLogicEvaluator: Boolean = true
}
case class UseLogicEvaluator() extends  LogicEvaluatorConfig {
  def needsLogicEvaluator: Boolean = true
}
