package leanprops
package cats

import arbitrary._

import _root_.cats.tests.CatsSuite
import _root_.cats.laws.discipline._
import _root_.cats.kernel.laws.discipline._

class CatsTestableTest extends CatsSuite {
  import Testable._

  checkAll("Testable.Result[Int]", FunctorTests[Result].functor[Int, Int, Int])
  checkAll("Testable.Result[Int]", EqTests[Result[Int]].eqv)
}
