package de.jens

import de.jens.expression._

case class Binding(val value: Map[Var, Value[_]]) {
	require(value != null)
	
//	override def equals(obj: Any) : Boolean = {
//	  true
//	}
}