package de.jens.expression

case class Or(first: Expression, second: Expression) extends Expression {
  require(first != null && second != null)
  
  override lazy val vars : Seq[Var] = first.vars.intersect(second.vars)
}