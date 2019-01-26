package leanprops
package cats

import arbitrary._

import _root_.cats.tests.CatsSuite
import _root_.cats.laws.discipline._
import _root_.cats.kernel.laws.discipline._

class CatsInspectableTest extends CatsSuite {
  import Inspectable._

  checkAll("Inspectable.Binding[Int]", FunctorTests[Binding].functor[Int, Int, Int])
  checkAll("Inspectable.Binding[Int]", EqTests[Binding[Int]].eqv)
}
