package de.jens
import de.jens.expression.Var
import de.jens.expression.Expression

class Rule {
  def <=(body : => Any) {
    body
  }
}

object Rule {
  
  private def predicate(name : String)
                       (subject : Expression, obj : Expression) : Expression = {
    new Expression() {}
  }
  
  def createPredicate(name : String) : (Expression, Expression) => Expression = {
    require(name != null && !name.isEmpty())
    return predicate(name) _
  }
  
  def define(vars : Var*)(head : => Any) : Rule = {
    head
    new Rule
  }
}