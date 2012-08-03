package de.jens
import de.jens.expression.Var
import de.jens.expression.Predicate

object Main extends Application {
  
  val solver = new Solver()

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
    isAge("Jens", x)
  }
  
  for( elem <- solver.resolve(isAge(x, 27))) print(elem)
  
  
}