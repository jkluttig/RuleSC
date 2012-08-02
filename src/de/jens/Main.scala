package de.jens
import de.jens.expression.Var

class Main {

  def isAge = Rule.createPredicate("isAge")
  val x = new Var("x")
  
  Rule.define(x) {
    isAge("Jens", x)
  }
  
}