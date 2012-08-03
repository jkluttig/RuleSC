package de.jens.index
import de.jens.expression.Predicate
import de.jens.expression.Var
import de.jens.expression.Value
import de.jens.Binding

abstract class Index {
  def store(subj : Value[_], pred : String, obj : Value[_])
  def scan(pred: Predicate) : Iterator[Map[Var, Value[_]]]
  
  protected def matches(pred: Predicate, value: Tuple3[Value[_], String, Value[_]]) : Boolean = {
    var ok = value._2 == pred.name
    pred.subj match {
        case x : Var => 
        case _ => ok &= value._1 == pred.subj
    }
    pred.obj match {
        case x : Var =>
        case _ => ok &= value._3 == pred.obj
    }
    ok
  }
  
  protected def createBinding(pred: Predicate, value: Tuple3[Value[_], String, Value[_]]) : Binding = {
    null
  }
}