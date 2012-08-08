package de.jens.test
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import de.jens.Solver
import de.jens.Binding
import de.jens.expression.Value

@RunWith(classOf[JUnitRunner])
class SingleRuleTest extends Specification {
  "With One Rule And one Predicate" should {
	  val solver = new Solver()
	  val x = solver.createVariable("x")
	  val y = solver.createVariable("y")
	  def ex = solver.createPredicate("ex")
	  def result = solver.createPredicate("result")
	  solver.defineFacts {
	    ex("Jens", 27)
	    ex("Jens", 28)
	  }
	  solver.define(x, y) {
	    result(x, y)
	  } <= {
	    ex(x, y)
	  }
	  "Rule Result is Same as Ground Result" in {
	    solver.resolve(ex(x, y)).toList mustEqual solver.resolve(result(x, y)).toList  
	  }
	  "Rule Result is Same as Ground Result Second" in {
	    solver.resolve(ex("Jens", 27)).toList mustEqual solver.resolve(result("Jens", 28)).toList  
	  }
  }
  "With One Rule And Conjunction" should {
	  val solver = new Solver()
	  val x = solver.createVariable("x")
	  val y = solver.createVariable("y")
	  val z = solver.createVariable("z")
	  def ex = solver.createPredicate("ex")
	  def result = solver.createPredicate("result")
	  solver.defineFacts {
	    ex("Jens", 27)
	    ex("Jens", 28)
	  }
	  solver.define(x, y, z) {
	    result(x, y)
	  } <= {
	    ex(x, y) && ex(x, 28)
	  }
	  "Rule Result is Same as Ground Result" in {
	    solver.resolve(result(x, y)).toList mustEqual solver.resolve(ex(x, y)).toList
	  }
	  "Rule Result is Same as Ground Result with Constant" in {
      solver.resolve(result(x, 27)).toList mustEqual solver.resolve(ex(x, 27)).toList
    }
	  //TODO: Tests mit Variablen umbinden, Variablen neudefinieren usw.
  }
}