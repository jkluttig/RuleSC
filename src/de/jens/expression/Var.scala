package de.jens.expression

class Var(name : String) extends Expression {
  require(name != null && !name.isEmpty())
}