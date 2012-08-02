package de.jens.expression

case class Value[A](value : A) extends Expression {
  require(value != null)
}