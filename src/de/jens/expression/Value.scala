package de.jens.expression

case class Value[A](value : A) extends Expression {
  require(value != null)
  
  override val vars : Seq[Var] = List()
}