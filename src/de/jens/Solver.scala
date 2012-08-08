package de.jens
import de.jens.expression._
import de.jens.index._
import de.jens.rule._
import java.util.HashSet

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
      evaluate(rules.get(pred.name).get, pred)
  }
  
  def project(vars: Seq[Var])(bind: Binding) : Binding = {
    val filtered = bind.value filter { (y) =>
        vars.contains(y._1)
      }
    Binding(filtered)
  }
  
  def evaluate(rule: Rule, goal: Predicate) : Iterator[Binding] = {
    evaluate(rule.body) map project(rule.head.vars) withFilter { (y) =>
      var ok = true
      var result = Map[Var, Value[_]]()
      rule.head.subj match {
        case x : Value[_] if (goal.subj.isInstanceOf[Value[_]]) => ok &= x == goal.subj
        case x : Var if (goal.subj.isInstanceOf[Value[_]]) => ok &= y.value.get(x).get == goal.subj
        case _ =>
      }
      rule.head.obj match {
        case x : Value[_] if (goal.obj.isInstanceOf[Value[_]]) => ok &= x == goal.obj
        case x : Var if (goal.obj.isInstanceOf[Value[_]]) => ok &= y.value.get(x).get == goal.obj
        case _ =>
      }
      ok
    } map { (binding) =>
      var result = Map[Var, Value[_]]()
      rule.head.subj match {
        case x : Value[_] if (goal.subj.isInstanceOf[Var]) => result += goal.subj.asInstanceOf[Var] -> x //Variable binden
        case x : Var if (goal.subj.isInstanceOf[Var]) => result += goal.subj.asInstanceOf[Var] -> binding.value.get(x).get //Variable umbinden
        case _ =>
      }
      rule.head.obj match {
        case x : Value[_] if (goal.obj.isInstanceOf[Var]) => result += goal.obj.asInstanceOf[Var] -> x //Variable binden
        case x : Var if (goal.obj.isInstanceOf[Var]) => result += goal.obj.asInstanceOf[Var] -> binding.value.get(x).get //Variable umbinden
        case _ =>
      }
      Binding(result)
    }
  }
  
  def evaluate(expr: Expression) : Iterator[Binding] = {
    expr match {
      case x: Predicate => evaluate(x)
      case x: And => evaluate(x)
      case x: Or => evaluate(x)
    }
  }
  
  def distinct() : Binding => Boolean = {
    val bindings = new HashSet[Binding]()
    return { (binding : Binding) =>
      if(bindings.contains(binding)){
        false
      } else {
        bindings.add(binding)
        true
      }
    }
  }
  
  def evaluate(or: Or) : Iterator[Binding] = {
    (evaluate(or.first) ++ evaluate(or.second)) withFilter distinct
  }
  
  def isJoin(first: Binding, second: Binding, vars: Seq[Var]) : Boolean = {
    for(variable <- vars){
      if(first.value.get(variable) != second.value.get(variable))
        return false
    }
    true
  }
  
  def evaluate(and: And) : Iterator[Binding] = {
    for(f <- evaluate(and.first); s <- evaluate(and.second) if isJoin(f, s, and.vars))
      yield Binding(f.value ++ s.value)
  }
  
  def evaluate(pred: Predicate) : Iterator[Binding] = {
    index.resolve(pred)
  }

}