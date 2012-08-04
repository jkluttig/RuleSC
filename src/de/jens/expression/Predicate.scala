package de.jens.expression

case class Predicate(name : String, subj : Expression, obj : Expression) extends Expression {
  require(name != null && !name.isEmpty())
  require(subj != null && obj != null)
  
  lazy val vars = for(i <- List(subj, obj) if i.isInstanceOf[Var]) yield i
}