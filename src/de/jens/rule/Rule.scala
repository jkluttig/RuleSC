package de.jens.rule
import de.jens.expression.Var
import de.jens.expression.Expression
import de.jens.expression.Predicate

class Rule(val head: Predicate) {
  require(head != null)
  
  var body : Expression = null
  
  def <=(f : => Expression) {
    body = f
    require(body != null)
  }
}