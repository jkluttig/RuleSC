package de.jens.test

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import de.jens.Solver
import de.jens.expression.Predicate


@RunWith(classOf[JUnitRunner])
class SimpleResolveTests extends Specification{
  
  "With no Facts defined" should {
      val solver = new Solver()
      def ex = solver.createPredicate("ex")
      "No Binding for concrete Predicate" in {
        solver.resolve(Predicate("ex", 1, 1)).isEmpty isTrue
      }
      
    }
  
}