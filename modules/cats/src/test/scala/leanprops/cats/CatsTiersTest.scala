package leanprops
package cats

import arbitrary._

import _root_.cats._
import _root_.cats.tests.CatsSuite
import _root_.cats.laws.discipline._
import _root_.cats.kernel.laws.discipline._

class CatsTiersTest extends CatsSuite {
  checkAll("Tiers[Int]", CoflatMapTests[Tiers].coflatMap[Int, Int, Int])
  checkAll("Tiers[Int] with Option", TraverseTests[Tiers].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Tiers[Int]", MonoidKTests[Tiers].monoidK[Int])
  checkAll("Tiers[Int]", MonoidTests[Tiers[Int]].monoid)
  checkAll("Tiers[Int]", EqTests[Tiers[Int]].eqv)
}
