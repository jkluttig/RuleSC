package de.jens.index

import de.jens._
import de.jens.expression._

abstract class Index {
	def store(subj: Value[_], pred: String, obj: Value[_])
	def resolve(pred: Predicate) : Iterator[Binding]
	
	protected def matches(pred: Predicate, tuple: Tuple3[Value[_],String,Value[_]]) : Boolean = {
	  var ok = tuple._2 == pred.name
      pred.subj match {
        case x : Var => 
        case _ => ok &= tuple._1 == pred.subj
      }
      pred.obj match {
        case x : Var =>
        case _ => ok &= tuple._3 == pred.obj
      }
      return ok
	}
	
	protected def createBinding(pred: Predicate, tuple: Tuple3[Value[_],String,Value[_]]) : Binding = {
	  var map = Map[Var, Value[_]]()
	  pred.subj match {
        case x : Var => map += x -> tuple._1
        case _ =>
      }
      pred.obj match {
        case x : Var => map += x -> tuple._3
        case _ =>
      }
      return Binding(map)
	}
}