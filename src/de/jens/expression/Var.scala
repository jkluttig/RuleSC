package de.jens.expression

case class Var(name : String) extends Expression {
  require(name != null && !name.isEmpty())
}