package de.jens.expression

case class And(first: Expression, second: Expression) extends Expression {
	require(first != null && second != null)
}