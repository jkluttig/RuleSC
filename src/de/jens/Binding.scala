package de.jens
import de.jens.expression.Var
import de.jens.expression.Value

case class Binding(value : Map[Var, Value[_]]) {
  require(value != null)
}