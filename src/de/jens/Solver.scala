package de.jens
import de.jens.expression.Expression
import de.jens.expression._
import de.jens.rule.Rule
import de.jens.index.SimpleIndex

class Solver {
  var collectFacts = false
  val index = new SimpleIndex()
  val rules = Map[String, Rule]()

  private def predicate(name: String)(subject: Expression, obj: Expression): Predicate = {
    if (collectFacts) {
      index.store(subject, name, obj)
    }
    Predicate(name, subject, obj)
  }

  def createPredicate(name: String): (Expression, Expression) => Predicate = {
    require(name != null && !name.isEmpty())
    predicate(name) _
  }

  def createVariable(name: String): Var = {
    Var(name)
  }

  def defineFacts(facts: => Any) {
    collectFacts = true
    facts
    collectFacts = false
  }

  def define(vars: Var*)(head: => Predicate): Rule = {
    head
    null
  }

  def resolve(pred: Predicate) : Set[Binding] = {
    require(pred != null)
    null
  }

}