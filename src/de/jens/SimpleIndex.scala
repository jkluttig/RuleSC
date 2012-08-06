package de.jens.index

import de.jens._
import de.jens.expression._

class SimpleIndex extends Index {
	val facts = scala.collection.mutable.Set[Tuple3[Value[_], String, Value[_]]]()
	
	override def store(subj: Value[_], pred: String, obj: Value[_]) {
	  facts add (subj, pred, obj)
	}
	
	override def resolve(pred: Predicate) : Iterator[Binding] = {
	  var iterator = facts toIterator;
	  for( x <- iterator if matches(pred, x)) yield createBinding(pred, x)
	}
}