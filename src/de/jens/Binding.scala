package de.jens

import de.jens.expression._

case class Binding(value: Map[Var, Value[_]]) {
	require(value != null)
}