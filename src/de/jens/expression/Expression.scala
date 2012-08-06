package de.jens.expression

abstract class Expression {
  
  def &&(expr: Expression) : And = {
    new And(this, expr)
  }
  
  def ||(expr: Expression) : Or = {
    new Or(this, expr)
  }

}

object Expression {
  implicit def toExpression(value : String) : Expression = {
    Value[String](value)
  }
  
  implicit def toExpression(value : Int) : Expression = {
    Value[Int](value)
  }
}