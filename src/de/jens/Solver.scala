package de.jens
import de.jens.expression.Expression
import de.jens.expression.Var
import de.jens.expression.Value
import scala.collection.mutable.MutableList
import de.jens.expression.Predicate
import scala.collection.mutable.HashMap$
import java.util.HashMap

class Solver {
  
  val factModel = MutableList[Tuple3[String, Expression, Expression]]()
  val rules = new HashMap[String, Rule]()
  
  private def predicate(name : String)
                       (subject : Expression, obj : Expression) = {
    factModel += Tuple3(name, subject, obj)
  }
  
  def createPredicate(name : String) : (Expression, Expression) => Any = {
    require(name != null && !name.isEmpty())
    predicate(name) _
  }
  
  def createVariable(name : String) : Var = {
    Var(name)
  }
  
  def defineFacts(facts : => Unit) {
    facts
  }
  
  def define(vars : Var*)(head : => Any) : Rule = {
    head
    new Rule
  }
  
  def resolve(pred : Predicate) : List[Map[Var, Value[_]]] = {
    require(pred != null)
    if (rules.isEmpty()) {
      factModel filter {
        _._1 == pred.name
      } map {
        Map()
      }
    }
    null
  }
  
}