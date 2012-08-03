package de.jens
import de.jens.expression.Expression
import de.jens.expression.Var
import de.jens.expression.Value
import de.jens.expression.Predicate
import de.jens.index._

class Solver {
  var collectFacts = false
  val index = new SimpleIndex()
  val rules = Map[String, Rule]()

  private def predicate(name: String)(subject: Expression, obj: Expression): Predicate = {
    if (collectFacts) {
      index.store(subject.asInstanceOf[Value[_]], name, obj.asInstanceOf[Value[_]])
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
    new Rule
  }

  def resolve(pred: Predicate) : Iterator[Binding] = {
    require(pred != null)
    index.resolve(pred)
  }

}