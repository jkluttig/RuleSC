package de.jens
import de.jens.index._

class BackwardEngine[TIndex <: Index](implicit m: scala.reflect.Manifest[TIndex]) extends Engine[TIndex] {

}