package de.jens
import de.jens.expression.Var
import de.jens.expression.Expression

class Rule {
  var body : Expression = null
  
  def <=(f : => Expression) {
    body = f
  }
}