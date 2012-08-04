package de.jens
import de.jens.expression._
import de.jens.index._
import de.jens.rule._

class Solver {
  var collectFacts = false
  val index = new SimpleIndex()
  var rules = Map[String, Rule]()

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
    var headPredicate = head
    rules += headPredicate.name -> new Rule(head)
    rules(headPredicate.name)
  }

  def resolve(pred: Predicate) : Iterator[Binding] = {
    require(pred != null)
    require(!(pred.subj.isInstanceOf[Var] && pred.obj.isInstanceOf[Var] && pred.obj == pred.subj))
    if(!rules.contains(pred.name))
    	index.resolve(pred)
    else
      evaluate(rules.get(pred.name).get) map { (x) => 
      val filtered = x.value filter { (y) =>
        pred.vars.contains(y._1)
      }
      Binding(filtered)
    }
  }
  
  def evaluate(rule: Rule) : Iterator[Binding] = {
    val body = evaluate(rule.body)
    body map { (x) =>
      val filtered = x.value filter { (y) =>
        println(rule.head.vars, y._1)
        rule.head.vars.contains(y._1)
      }
      Binding(filtered)
    }
  }
  
  def evaluate(expr: Expression) : Iterator[Binding] = {
    expr match {
      case x: Predicate => evaluate(x)
    }
  }
  
  def evaluate(pred: Predicate) : Iterator[Binding] = {
    index.resolve(pred)
  }

}