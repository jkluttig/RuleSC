package de.jens
import de.jens.expression.Var
import de.jens.expression.Predicate
import de.jens.index._

object Main extends Application {
  
  class A[B <: Index](implicit m: scala.reflect.Manifest[B]) {
    var b : B = m.erasure.newInstance.asInstanceOf[B]
  }
  
  var solver = new Solver() with ForwardEngine
  
  solver.hello()

  def isAge = solver.createPredicate("isAge")
  def alterIst = solver.createPredicate("alterIst")
  
  solver.defineFacts {
    isAge("Jens", 28)
    isAge("Sven", 27)
  }
  
  val x = solver.createVariable("x")
  
  solver.define(x) {
    alterIst("Jens", x)
  } <= {
    isAge("Jens", x) && isAge("Sven", x)
  }
  
  //for( elem <- solver.resolve(isAge(x, 27))) print(elem)
  
  
}