package de.jens.test

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import de.jens.Solver
import de.jens.expression._
import de.jens.Binding


@RunWith(classOf[JUnitRunner])
class SimpleResolveTests extends Specification{
  
  "With no Facts defined" should {
      val solver = new Solver()
      val x = solver.createVariable("x")
      def ex = solver.createPredicate("ex")
      "No Binding for concrete Predicate" in {
        solver.resolve(ex(1, 1)).isEmpty
      }
      "Using Variable twice throws Exception" in {
        solver.resolve(ex(x, x)) should throwA[Exception]
      }
    }
  
  "With Only Facts defined" should {
      val solver = new Solver()
      val x = solver.createVariable("x")
      def ex = solver.createPredicate("ex")
      def sample = solver.createPredicate("sample")
      solver.defineFacts {
    	ex("Jens", 27)
    	ex("Jens", 27)
    	ex("Sven", 27)
    	sample(1, 5)
    	sample(1, 10)
      }
      "No Binding for Name Jochen" in {
        solver.resolve(ex("Jochen", x)).isEmpty
      }
      val resOne = solver.resolve(ex(x, 27)).toList
      "Age 27 are two Results And Correct Bindings" in {
        (resOne.size mustEqual 2) and (resOne.contains(Binding(Map(x -> Value("Jens")))) mustEqual true) and (resOne.contains(Binding(Map(x -> Value("Sven")))) mustEqual true)
      }
      val resTwo = solver.resolve(ex("Jens", x)).toList
      "Name Jens are one Result" in {
        (resTwo.size mustEqual 1) and (resTwo.contains(Binding(Map(x -> Value(27)))) mustEqual true)
      }
      val resThree = solver.resolve(sample(1, x)).toList
      "Other Predicate has two Results" in {
        (resThree.size mustEqual 2) and (resThree.contains(Binding(Map(x -> Value(5)))) mustEqual true) and (resThree.contains(Binding(Map(x -> Value(10)))) mustEqual true)
      }
      val resFour = solver.resolve(sample(1, 5)).toList
      "Without Vars but Predicate exists" in {
        (resFour.isEmpty mustEqual false) and (resFour.contains(Binding(Map())) mustEqual true)
      }
      "Without Vars but not existing" in {
        solver.resolve(ex("nix", 1)).isEmpty
      }
    }
  
}