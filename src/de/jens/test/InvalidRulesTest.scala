package de.jens.test
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import de.jens.Solver

@RunWith(classOf[JUnitRunner])
class InvalidRulesTest extends Specification {
  
  "Rule with Ivalid Vars" should {
    val solver = new Solver()
    val x = solver.createVariable("x")
    val y = solver.createVariable("y")
    def ex = solver.createPredicate("ex")
    def ex2 = solver.createPredicate("ex2")
    def ex3 = solver.createPredicate("ex3")
    solver.define(x, y) {
      ex(x, "Nix")
    } <= {
      ex(x, 27)
    }
    "Throw Exception cause unused Vars in Declaratin" in {
      solver.validate should throwA[Exception]
    }
    solver.clearRules
    solver.define(x, y) {
      ex2(x, "Nix")
    } <= {
      ex(y, 27)
    }
    "Throw Exception cause Var used in Head not used in body" in {
      solver.validate should throwA[Exception]
    }
    solver.clearRules
    solver.define(x, y) {
      ex3(x, "Nix")
    } <= {
      ex(y, 27)
    }
    "Throw Exception cause Var used in Head not used in body" in {
      solver.validate should throwA[Exception]
    }
  }

}