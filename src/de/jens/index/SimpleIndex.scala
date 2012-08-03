package de.jens.index
import de.jens.expression._
import scala.collection.mutable._
import de.jens.Binding

class SimpleIndex extends Index {
  val facts = new scala.collection.mutable.HashSet[Tuple3[Value[_], String, Value[_]]]
  
  override def store(subj : Value[_], pred : String, obj : Value[_]) {
    facts add (subj, pred, obj)
  }
  
  override def scan(pred: Predicate) : Iterator[Binding] = {
    var iterator = facts toIterator;
    for(i <- iterator if matches(pred, i)) yield createBinding(pred, i)
  }
  
}