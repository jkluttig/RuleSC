package de.jens
import de.jens.index._
import de.jens.expression._
import de.jens.rule.Rule

abstract class Engine[TIndex <: Index](implicit m: scala.reflect.Manifest[TIndex]) {
  var collectFacts = false
  val index = m.erasure.newInstance.asInstanceOf[TIndex]
  var rules = Map[String, Rule]()

  def createPredicate(name: String): (Expression, Expression) => Predicate = {
    require(name != null && !name.isEmpty())
    predicate(name) _
  }
  
  protected def predicate(name: String)(subject: Expression, obj: Expression): Predicate = {
    if (collectFacts) {
      index.store(subject.asInstanceOf[Value[_]], name, obj.asInstanceOf[Value[_]])
    }
    Predicate(name, subject, obj)
  }
  
  def defineFacts(facts: => Any) {
    collectFacts = true
    facts
    collectFacts = false
  }

  def define(vars: Var*)(head: => Predicate): Rule = {
    var headPredicate = head
    rules += headPredicate.name -> new Rule(head)
    rules(headPredicate.name)
  }
  
  def validate() {
    
  }
  
  def clearRules() {
    rules = Map[String, Rule]()
  }
  
}