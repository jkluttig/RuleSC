package de.jens.expression

trait Expression {

}

object Expression {
  implicit def toExpression(value : String) : Expression = {
    Value[String](value)
  }
  
  implicit def toExpression(value : Int) : Expression = {
    Value[Int](value)
  }
}