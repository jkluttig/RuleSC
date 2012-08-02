package de.jens
import de.jens.expression.Expression
import de.jens.expression.Var
import de.jens.expression.Value
import de.jens.expression.Predicate

class Solver {
  var collectFacts = false
  val factModel = scala.collection.mutable.Set[Tuple3[String, Expression, Expression]]()
  val rules = Map[String, Rule]()

  private def predicate(name: String)(subject: Expression, obj: Expression): Predicate = {
    if (collectFacts) {
      factModel add Tuple3(name, subject, obj)
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

  def resolve(pred: Predicate) : scala.collection.mutable.Set[Map[Var, Value[_]]] = {
    require(pred != null)
    factModel filter { (it) =>
      var ok = it._1 == pred.name
      pred.subj match {
        case x : Var => 
        case _ => ok &= it._2 == pred.subj
      }
      pred.obj match {
        case x : Var =>
        case _ => ok &= it._3 == pred.obj
      }
      ok
    } map { (a) =>
      pred.subj match {
        case x : Var => 
        case _ => ok &= it._2 == pred.subj
      }
      Map((Var("x"), Value(1)))
    }
  }

}